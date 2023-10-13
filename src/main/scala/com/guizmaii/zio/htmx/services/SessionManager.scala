package com.guizmaii.zio.htmx.services

import com.guizmaii.zio.htmx.AppConfig
import com.guizmaii.zio.htmx.domain.User
import com.guizmaii.zio.htmx.types.CookieSignKey
import zio.http.*
import zio.json.{DecoderOps, DeriveJsonCodec, EncoderOps, JsonCodec}
import zio.{Cause, Duration, Trace, URLayer, ZEnvironment, ZIO, ZLayer, durationInt, durationLong}

import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.Base64
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
  @inline def id: String        = sub
  @inline def firstName: String = given_name
  @inline def lastName: String  = family_name
  @inline def fullName: String  = name
}
object IdToken                                                                                                                       {
  // TODO Jules
  @nowarn
  def decode(s: String): IdToken =
    IdToken(
      sub = "kp_0c6149688c8ded448a8dbe0aed4c6bac",
      given_name = "Jules",
      family_name = "Ivanic",
      name = "Jules Ivanic",
      email = "jules.ivanic@example.com",
      picture = Some("https://avatars.githubusercontent.com/u/1193670?v=4"),
    )
}

final case class SessionCookieContent(expiresAt: Instant, refreshToken: String, idToken: String)
object SessionCookieContent {
  implicit private val codec: JsonCodec[SessionCookieContent] = DeriveJsonCodec.gen[SessionCookieContent]

  // TODO: Needs encryption
  def encode(sessionCookieContent: SessionCookieContent): String =
    Base64.getUrlEncoder.encodeToString(sessionCookieContent.toJson.getBytes(StandardCharsets.UTF_8))

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

  def authMiddleware: HttpAppMiddleware.WithOut[Nothing, Any, Nothing, Throwable, λ[Env => Env & User], λ[A => Throwable]]

  def loggedUser(request: Request): Option[User]
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
        content = Base64.getUrlEncoder.encodeToString(state.getBytes(StandardCharsets.UTF_8)),
        maxAge = Some(1.hour), // This cookie is valid only 1h
        isSecure = false,      // TODO: should be `true` in Staging and Prod
        isHttpOnly = true,
        // Needs to be Lax otherwise the cookie isn't included in the `/auth/callback` request coming from Kinde
        sameSite = Some(Cookie.SameSite.Lax),
        path = Some(Path.root),
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
      try Some(new String(Base64.getUrlDecoder.decode(content), StandardCharsets.UTF_8))
      catch {
        case NonFatal(_) => None
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

  override def authMiddleware: HttpAppMiddleware.WithOut[Nothing, Any, Nothing, Throwable, λ[Env => Env & User], λ[A => Throwable]] =
    sessionCookieMiddleware[User](session =>
      try {
        val idToken = IdToken.decode(session.idToken)
        Right(User(firstName = idToken.firstName, lastName = idToken.lastName, email = idToken.email))
      } catch {
        case NonFatal(e) => Left(e)
      }
    )

  override def loggedUser(request: Request): Option[User] =
    getCookieContent(request, sessionCookieName)
      .flatMap(content =>
        SessionCookieContent.decode(content) match {
          case Left(_)        => None
          case Right(session) =>
            // TODO Jules: "refresh token" part is missing
            try {
              val idToken = IdToken.decode(session.idToken)
              Some(User(firstName = idToken.firstName, lastName = idToken.lastName, email = idToken.email))
            } catch {
              case NonFatal(_) => None
            }
        }
      )

  private def getCookieContent(request: Request, cookieName: String): Option[String] =
    request
      .header(Header.Cookie)
      .flatMap(_.value.find(_.name == cookieName))
      .flatMap(_.unSign(cookieSignKey))
      .filter(_.content.nonEmpty)
      .map(_.content)

  private def sessionCookieMiddleware[Context: zio.Tag](
    extractContext: SessionCookieContent => Either[Throwable, Context]
  ): HttpAppMiddleware.WithOut[Nothing, Any, Nothing, Throwable, λ[Env => Env & Context], λ[A => Throwable]] =
    new HttpAppMiddleware.Contextual[Nothing, Any, Nothing, Throwable] {
      type OutEnv[Env] = Env & Context
      type OutErr[_]   = Throwable

      override def apply[Env, Err <: Throwable](
        http: Http[Env, Err, Request, Response]
      )(implicit trace: Trace): Http[Env & Context, Throwable, Request, Response] = {

        def providedHttp(context: Context): Http[Env & Context, Err, Request, Response] =
          http.provideSomeEnvironment[Env](_.union[Context](ZEnvironment(context)))

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

                if (sessionStillValid) {
                  extractContext(session) match {
                    case Right(context) => ZIO.succeed(providedHttp(context))
                    case Left(error)    =>
                      ZIO.logErrorCause("Error while extracting the context from the session cookie", Cause.fail(error)) *>
                        ZIO.succeed(
                          Http.fromHandler(
                            Handler.response(
                              // TODO: Is `Unauthorized` appropriate here?
                              // TODO: Should we really wipe the session cookie here?
                              Response.status(Status.Unauthorized).addCookie(sessionInvalidationCookie)
                            )
                          )
                        )
                  }
                } else {
                  (
                    for {
                      response  <- identityProvider.refreshTokens(session.refreshToken)
                      newSession = SessionCookieContent(
                                     expiresAt = Instant.now().plusSeconds(response.expiresIn),
                                     refreshToken = response.refreshToken,
                                     idToken = response.idToken,
                                   )
                      r         <- extractContext(newSession) match {
                                     case Right(context) =>
                                       ZIO.succeed {
                                         providedHttp(context).map(
                                           _.addCookie(newSessionCookie(newSession, response.expiresIn.seconds))
                                         )
                                       }
                                     case Left(error)    =>
                                       ZIO.logErrorCause("Error while extracting the context from the session cookie", Cause.fail(error)) *>
                                         ZIO.succeed {
                                           Http.fromHandler(
                                             Handler.response(
                                               // TODO: Is `Unauthorized` appropriate here?
                                               // TODO: Should we really wipe the session cookie here?
                                               Response.status(Status.Unauthorized).addCookie(sessionInvalidationCookie)
                                             )
                                           )
                                         }
                                   }
                    } yield r
                  ).logError("Error while refreshing the tokens")
                    .catchAll(_ =>
                      ZIO.succeed {
                        Http.fromHandler(
                          Handler.response(
                            // TODO: /sign-in URL isn't the correct one here
                            Response.redirect(URL.root.withPath("/sign-in")).addCookie(sessionInvalidationCookie)
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

}
