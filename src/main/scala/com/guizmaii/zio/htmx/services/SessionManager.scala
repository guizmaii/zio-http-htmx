package com.guizmaii.zio.htmx.services

import com.guizmaii.zio.htmx.AppConfig
import com.guizmaii.zio.htmx.domain.LoggedUser
import com.guizmaii.zio.htmx.types.CookieSignKey
import zio.http.*
import zio.json.{DecoderOps, DeriveJsonCodec, EncoderOps, JsonCodec}
import zio.{Duration, Trace, URLayer, ZEnvironment, ZIO, ZLayer, durationInt, durationLong}

import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.Base64
import java.util.regex.Pattern
import scala.annotation.nowarn
import scala.util.control.NonFatal

/**
 * Raw id_token from Kinde contains:
 * {{{
 *  {
 *    "at_hash": "sWoDGICu3YcYX9_tC0QIdE",
 *    "aud": [
 *      "https://<my-app>.kinde.com",
 *      "844e057836fd4e1fbb2c92507c58add2"
 *    ],
 *    "auth_time": 1697184156,
 *    "azp": "844e057836fd4e1fbb2c92507c58add2",
 *    "email": "jules.ivanic@gmail.com",
 *    "exp": 1697187756,
 *    "family_name": "Ivanic",
 *    "given_name": "Jules",
 *    "iat": 1697185641,
 *    "iss": "https://<my-app>.kinde.com",
 *    "jti": "e341aa0d-407a-4689-980a-039d14d0b3fb",
 *    "name": "Jules Ivanic",
 *    "org_codes": [
 *      "org_576a383bb64"
 *    ],
 *    "picture": "https://avatars.githubusercontent.com/u/1193670?v=4",
 *    "sub": "kp_0c6149688c8ded448a8dbe0aed4c6bac",
 *    "updated_at": 1697125145
 * }
 * }}}
 *
 * See also: https://kinde.com/docs/build/about-id-tokens/
 *
 * Additional info:
 *  - `picture` can be `null` according to the TS SDK
 *  - `sub` is the user ID
 */
final case class IdToken(sub: String, given_name: String, family_name: String, name: String, email: String, picture: Option[String]) {
  @inline def userId: String    = sub
  @inline def firstName: String = given_name
  @inline def lastName: String  = family_name
  @inline def fullName: String  = name
}
object IdToken                                                                                                                       {
  implicit val decoder: JsonCodec[IdToken] = DeriveJsonCodec.gen[IdToken]

  /**
   * Split a string on the `.` character
   */
  private val headerRegex: Pattern = Pattern.compile("\\.")

  def decode(raw: String): Either[String, IdToken] =
    headerRegex.split(raw, 3) match {
      case Array(_, claims, _) =>
        try new String(Base64.getUrlDecoder.decode(claims.getBytes(StandardCharsets.UTF_8)), StandardCharsets.UTF_8).fromJson[IdToken]
        catch {
          case NonFatal(_) => Left("Invalid id_token: Unable to decode claims")
        }
      case _                   => Left("Invalid id_token: Token does not match the correct pattern")
    }
}

final case class SignInCookieContent(expiresAt: Instant, state: String)
object SignInCookieContent {
  implicit private val codec: JsonCodec[SignInCookieContent] = DeriveJsonCodec.gen[SignInCookieContent]

  def make(state: String): SignInCookieContent =
    SignInCookieContent(
      expiresAt = Instant.now().plusSeconds(3600), // 1 hour of validity
      state = state,
    )

  // TODO: Needs encryption
  def encode(signInCookieContent: SignInCookieContent): String =
    new String(Base64.getUrlEncoder.encode(signInCookieContent.toJson.getBytes(StandardCharsets.UTF_8)), StandardCharsets.UTF_8)

  // TODO: Needs encryption
  def decode(s: String): Either[String, SignInCookieContent] =
    try new String(Base64.getUrlDecoder.decode(s), StandardCharsets.UTF_8).fromJson[SignInCookieContent]
    catch {
      case NonFatal(_) => Left("Invalid cookie encoding")
    }
}

@nowarn("cat=scala3-migration")
final case class SessionCookieContent private (expiresAt: Instant, refreshToken: String, loggedUser: LoggedUser)
object SessionCookieContent {
  implicit private val codec: JsonCodec[SessionCookieContent] = DeriveJsonCodec.gen[SessionCookieContent]

  def from(codeTokenResponse: CodeTokenResponse): Either[String, SessionCookieContent] =
    IdToken.decode(codeTokenResponse.idToken).map { idToken =>
      SessionCookieContent(
        expiresAt = Instant.now().plusSeconds(codeTokenResponse.expiresIn),
        refreshToken = codeTokenResponse.refreshToken,
        loggedUser = LoggedUser.from(idToken),
      )
    }

  // TODO: Needs encryption
  def encode(sessionCookieContent: SessionCookieContent): String =
    new String(Base64.getUrlEncoder.encode(sessionCookieContent.toJson.getBytes(StandardCharsets.UTF_8)), StandardCharsets.UTF_8)

  // TODO: Needs decryption
  def decode(s: String): Either[String, SessionCookieContent] =
    try new String(Base64.getUrlDecoder.decode(s), StandardCharsets.UTF_8).fromJson[SessionCookieContent]
    catch {
      case NonFatal(_) => Left("Invalid cookie encoding")
    }
}

//noinspection NonAsciiCharacters
trait SessionManager {
  def signInCookie(state: String): Cookie.Response
  def signInInvalidationCookie: Cookie.Response
  def getSignInState(request: Request): Option[String]

  def newSessionCookie(newSession: SessionCookieContent, expiresIn: Duration): Cookie.Response
  def sessionInvalidationCookie: Cookie.Response

  def authMiddleware: HttpAppMiddleware.WithOut[Nothing, Any, Nothing, Throwable, λ[Env => Env & LoggedUser], λ[A => Throwable]]

  def loggedUser(request: Request): Option[LoggedUser]
}

object SessionManager {
  def live: URLayer[IdentityProvider & AppConfig, SessionManagerLive] =
    ZLayer.fromZIO {
      for {
        appConfig <- ZIO.service[AppConfig]
        idp       <- ZIO.service[IdentityProvider]
      } yield new SessionManagerLive(appConfig.cookieSignKey, idp)
    }
}

//noinspection NonAsciiCharacters
final class SessionManagerLive(cookieSignKey: CookieSignKey, identityProvider: IdentityProvider) extends SessionManager {
  private val signInCookieName: "HTMX_SIGN_IN"  = "HTMX_SIGN_IN"
  private val sessionCookieName: "HTMX_SESSION" = "HTMX_SESSION"

  override def signInCookie(state: String): Cookie.Response =
    Cookie
      .Response(
        name = signInCookieName,
        content = SignInCookieContent.encode(SignInCookieContent.make(state)),
        maxAge = Some(1.hour), // This cookie is valid only 1h
        isSecure = false,      // TODO: should be `true` in Staging and Prod
        isHttpOnly = true,
        // Needs to be Lax otherwise the cookie isn't included in the `/auth/callback` request coming from Kinde
        sameSite = Some(Cookie.SameSite.Lax),
        path = Some(Path.root),// TODO: Should be `/auth`?
      )
      .sign(cookieSignKey)

  override val signInInvalidationCookie: Cookie.Response =
    Cookie
      .Response(
        name = signInCookieName,
        content = "",
        maxAge = Some(Duration.Zero), // expires the session cookie
        isSecure = false,             // TODO: should be `true` in Staging and Prod
        isHttpOnly = true,
        sameSite = Some(Cookie.SameSite.Lax),
        path = Some(Path.root),
      )
      .sign(cookieSignKey)

  override def newSessionCookie(newSession: SessionCookieContent, expiresIn: Duration): Cookie.Response =
    Cookie
      .Response(
        name = sessionCookieName,
        content = SessionCookieContent.encode(newSession),
        maxAge = Some(expiresIn),
        isSecure = false, // TODO: should be `true` in Staging and Prod
        isHttpOnly = true,
        // Needs to be Lax otherwise the cookie isn't included in `/` request coming from Kinde after login
        sameSite = Some(Cookie.SameSite.Lax),
        path = Some(Path.root),
      )
      .sign(cookieSignKey)

  override def getSignInState(request: Request): Option[String] =
    getCookieContent(request, signInCookieName).flatMap { content =>
      SignInCookieContent.decode(content) match {
        case Left(_)                                      => None
        case Right(SignInCookieContent(expiresAt, state)) =>
          val notExpired = Instant.now().isBefore(expiresAt)
          if (notExpired) Some(state) else None
      }
    }

  override val sessionInvalidationCookie: Cookie.Response =
    Cookie
      .Response(
        name = sessionCookieName,
        content = "",
        maxAge = Some(Duration.Zero), // expires the session cookie
        isSecure = false,             // TODO: should be `true` in Staging and Prod
        isHttpOnly = true,
        sameSite = Some(Cookie.SameSite.Lax),
        path = Some(Path.root),
      )
      .sign(cookieSignKey)

  override val authMiddleware: HttpAppMiddleware.WithOut[Nothing, Any, Nothing, Throwable, λ[Env => Env & LoggedUser], λ[A => Throwable]] =
    new HttpAppMiddleware.Contextual[Nothing, Any, Nothing, Throwable] {
      type OutEnv[Env] = Env & LoggedUser
      type OutErr[_]   = Throwable

      override def apply[Env, Err <: Throwable](
        http: Http[Env, Err, Request, Response]
      )(implicit trace: Trace): Http[Env & LoggedUser, Throwable, Request, Response] = {

        def providedHttp(context: LoggedUser): Http[Env & LoggedUser, Err, Request, Response] =
          http.provideSomeEnvironment[Env](_.union[LoggedUser](ZEnvironment(context)))

        Http.fromHttpZIO { request =>
          ZIO.suspendSucceed {
            getCookieContent(request, sessionCookieName)
              .toRight("Missing or invalid session cookie")
              .flatMap(content => SessionCookieContent.decode(content)) match {

              case Left(e) =>
                ZIO
                  .logError(s"Invalid session cookie: $e")
                  .as(
                    Http.fromHandler(
                      Handler.response(
                        Response.status(Status.Unauthorized).addCookie(sessionInvalidationCookie)
                      )
                    )
                  )

              case Right(session) =>
                val sessionStillValid = Instant.now().isBefore(session.expiresAt)
                if (sessionStillValid) ZIO.succeed(providedHttp(session.loggedUser))
                else {
                  (
                    for {
                      response <- identityProvider.refreshTokens(session.refreshToken)
                      r        <- ZIO.suspendSucceed {
                                    SessionCookieContent.from(response) match {
                                      case Right(newSession) =>
                                        val newCookie  = newSessionCookie(newSession, response.expiresIn.seconds)
                                        val loggedUser = newSession.loggedUser

                                        ZIO.succeed(providedHttp(loggedUser).map(_.addCookie(newCookie)))
                                      case Left(e)           =>
                                        ZIO.logError(s"Error while decoding the new session cookie: $e") *>
                                          ZIO.fail(e)
                                    }
                                  }
                    } yield r
                  ).logError("Error while refreshing the tokens")
                    .catchAll(_ =>
                      ZIO.succeed {
                        val (signInUri, state) = identityProvider.getSignInUrl

                        Http.fromHandler(
                          Handler.response(
                            Response
                              .redirect(signInUri)
                              .addCookie(sessionInvalidationCookie)
                              .addCookie(signInCookie(state))
                          )
                        )
                      }
                    )
                }
            }
          }
        }
      }
    }

  override def loggedUser(request: Request): Option[LoggedUser] =
    getCookieContent(request, sessionCookieName)
      .flatMap(content =>
        SessionCookieContent.decode(content) match {
          case Left(_)        => None
          case Right(session) =>
            // TODO Jules: "refresh token" part is missing
            Some(session.loggedUser)
        }
      )

  private def getCookieContent(request: Request, cookieName: String): Option[String] =
    request
      .header(Header.Cookie)
      .flatMap(_.value.find(_.name == cookieName))
      .flatMap(_.unSign(cookieSignKey))
      .filter(_.content.nonEmpty)
      .map(_.content)

}
