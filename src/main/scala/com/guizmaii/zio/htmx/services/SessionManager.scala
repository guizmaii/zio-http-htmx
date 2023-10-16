package com.guizmaii.zio.htmx.services

import com.guizmaii.zio.htmx.AppConfig
import com.guizmaii.zio.htmx.domain.LoggedUser
import com.guizmaii.zio.htmx.persistence.{Session, SessionStorage}
import com.guizmaii.zio.htmx.types.CookieSignKey
import zio.http.*
import zio.json.{DecoderOps, DeriveJsonCodec, EncoderOps, JsonCodec}
import zio.prelude.ForEachOps
import zio.{Cause, Duration, Random, Schedule, Task, Trace, UIO, URLayer, ZEnvironment, ZIO, ZLayer, durationInt, durationLong}

import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.regex.Pattern
import java.util.{Base64, UUID}
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

@nowarn("cat=scala3-migration")
final case class SessionCookieContent private (refreshToken: String, loggedUser: LoggedUser)
object SessionCookieContent {
  implicit val codec: JsonCodec[SessionCookieContent] = DeriveJsonCodec.gen[SessionCookieContent]

  private[services] def from(codeTokenResponse: CodeTokenResponse): Either[String, SessionCookieContent] =
    IdToken.decode(codeTokenResponse.idToken).map { idToken =>
      SessionCookieContent(
        refreshToken = codeTokenResponse.refreshToken,
        loggedUser = LoggedUser.from(idToken),
      )
    }
}

sealed trait SessionState extends Product with Serializable {
  def asSome[A](f: SessionCookieContent => A): Option[A] =
    this match {
      case SessionState.NoSession      => None
      case SessionState.Expired        => None
      case SessionState.Valid(session) => Some(f(session))
    }
}
object SessionState {
  case object NoSession                                 extends SessionState
  case object Expired                                   extends SessionState
  final case class Valid(session: SessionCookieContent) extends SessionState
}

//noinspection NonAsciiCharacters
trait SessionManager {
  def signInUrl: Task[(URL, Cookie.Response)]
  def logoutUrl(request: Request): UIO[(URL, Cookie.Response, Cookie.Response)]
  def newUserSession(request: Request): Task[(Cookie.Response, Cookie.Response)]
  def loggedUser(request: Request): UIO[Option[LoggedUser]]

  def authMiddleware: HttpAppMiddleware.WithOut[Nothing, Any, Nothing, Throwable, λ[Env => Env & LoggedUser], λ[A => Throwable]]
}

object SessionManager {
  def live: URLayer[SessionStorage & IdentityProvider & AppConfig, SessionManagerLive] =
    ZLayer.fromZIO {
      for {
        appConfig      <- ZIO.service[AppConfig]
        idp            <- ZIO.service[IdentityProvider]
        sessionStorage <- ZIO.service[SessionStorage]
        random         <- ZIO.random
      } yield new SessionManagerLive(appConfig.cookieSignKey, idp, sessionStorage, random)
    }
}

//noinspection NonAsciiCharacters
final class SessionManagerLive(
  cookieSignKey: CookieSignKey,
  identityProvider: IdentityProvider,
  sessionStorage: SessionStorage,
  random: Random,
) extends SessionManager {
  private val signInCookieName: "HTMX_SIGN_IN"      = "HTMX_SIGN_IN"
  private val userSessionCookieName: "HTMX_SESSION" = "HTMX_SESSION"

  private val signInSessionInvalidationCookie: Cookie.Response =
    Cookie
      .Response(
        name = signInCookieName,
        content = "",
        maxAge = Some(Duration.Zero), // expires the session cookie
        isSecure = false,             // TODO: should be `true` in Staging and Prod
        isHttpOnly = true,
        sameSite = Some(Cookie.SameSite.Lax),
        path = Some(Path.root / "auth"),
      )
      .sign(cookieSignKey)

  private val userSessionInvalidationCookie: Cookie.Response =
    Cookie
      .Response(
        name = userSessionCookieName,
        content = "",
        maxAge = Some(Duration.Zero), // expires the session cookie
        isSecure = false,             // TODO: should be `true` in Staging and Prod
        isHttpOnly = true,
        sameSite = Some(Cookie.SameSite.Lax),
        path = Some(Path.root),
      )
      .sign(cookieSignKey)

  override def signInUrl: Task[(URL, Cookie.Response)] =
    (
      for {
        (signInUrl, state)  <- ZIO.succeed(identityProvider.getSignInUrl)
        signInSessionCookie <- newSignInSession(state)
      } yield (signInUrl, signInSessionCookie)
    ).logError("Error while creating the sign-in session")

  override def logoutUrl(request: Request): UIO[(URL, Cookie.Response, Cookie.Response)] =
    for {
      signInSessionInvalidationCookie <- invalidateSignInSession(request)
      userSessionInvalidationCookie   <- invalidateUserSession(request)
    } yield (identityProvider.logoutUrl, signInSessionInvalidationCookie, userSessionInvalidationCookie)

  override def newUserSession(request: Request): Task[(Cookie.Response, Cookie.Response)] = {
    def signInState(request: Request): UIO[Option[String]] =
      ZIO
        .suspendSucceed {
          sessionId(request, signInCookieName) match {
            case None     => ZIO.none
            case Some(id) =>
              sessionStorage
                .get(id)
                .retry(Schedule.fixed(100.millis) && Schedule.recurs(3).unit)
                .foldZIO(
                  failure = e =>
                    ZIO
                      .logFatalCause("Failed to get the sign-in session from the storage", Cause.fail(e))
                      .as(None),
                  success = {
                    case None          => ZIO.none
                    case Some(session) =>
                      if (session.notExpired(Instant.now())) ZIO.some(session.content)
                      else {
                        // We can ignore here as the session cookie will be wiped out anyway
                        sessionStorage.invalidate(id).ignore.as(None)
                      }
                  },
                )
          }
        }

    def newSession(codeTokenResponse: CodeTokenResponse): Task[Cookie.Response] =
      (
        for {
          session   <- ZIO
                         .fromEither(SessionCookieContent.from(codeTokenResponse))
                         .flatMapError(e => ZIO.logFatal(s"Invalid code token response: $e").as(new RuntimeException(e)))
          sessionId <- random.nextUUID
          expiresIn  = codeTokenResponse.expiresIn.seconds
          _         <- sessionStorage
                         .store(sessionId, Session.make(session.toJson, expiresIn))
                         .retry(Schedule.fixed(100.millis) && Schedule.recurs(3).unit)
                         .tapErrorCause(e => ZIO.logFatalCause("Failed to store the user session in the session storage", Cause.fail(e)))
        } yield Cookie
          .Response(
            name = userSessionCookieName,
            content = sessionId.toString,
            maxAge = Some(expiresIn),
            isSecure = false, // TODO: should be `true` in Staging and Prod
            isHttpOnly = true,
            // Needs to be Lax otherwise the cookie isn't included in `/` request coming from Kinde after login
            sameSite = Some(Cookie.SameSite.Lax),
            path = Some(Path.root),
          )
          .sign(cookieSignKey)
      ).logError("Error while creating the session")

    for {
      maybeSavedState <- signInState(request)
      maybeState       = request.url.queryParams.get("state").flatMap(_.headOption.filter(_.nonEmpty))
      maybeCode        = request.url.queryParams.get("code").flatMap(_.headOption.filter(_.nonEmpty))
      cookies         <- (maybeCode, maybeSavedState, maybeState) match {
                           case (Some(code), Some(savedState), Some(state)) if savedState == state =>
                             for {
                               response                        <- identityProvider.handleSignIn(code)
                               newSessionCookie                <- newSession(response)
                               signInSessionInvalidationCookie <- invalidateSignInSession(request)
                             } yield (newSessionCookie, signInSessionInvalidationCookie)
                           case _                                                                  =>
                             ZIO.fail(new RuntimeException("Invalid state")) // TODO Jules: can we do better?
                         }
    } yield cookies
  }

  // TODO: The invalidation of the session is missing here
  override def loggedUser(request: Request): UIO[Option[LoggedUser]] =
    getSessionContent(request).map(_.asSome(_.loggedUser))

  override val authMiddleware: HttpAppMiddleware.WithOut[Nothing, Any, Nothing, Throwable, λ[Env => Env & LoggedUser], λ[A => Throwable]] =
    new HttpAppMiddleware.Contextual[Nothing, Any, Nothing, Throwable] {
      type OutEnv[Env] = Env & LoggedUser
      type OutErr[_]   = Throwable

      override def apply[Env, Err <: Throwable](
        http: Http[Env, Err, Request, Response]
      )(implicit trace: Trace): Http[Env & LoggedUser, Throwable, Request, Response] = {

        def providedHttp(context: LoggedUser): Http[Env & LoggedUser, Throwable, Request, Response] =
          http.provideSomeEnvironment[Env](_.union[LoggedUser](ZEnvironment(context)))

        Http.fromHttpZIO { request =>
          getSessionContent(request).map {
            case SessionState.Valid(session)                                    => providedHttp(session.loggedUser)
            case sessionState @ (SessionState.NoSession | SessionState.Expired) =>
              Http.fromHandler {
                Handler.responseZIO {
                  for {
                    _                   <- ZIO.unit
                    (signInUri, state)   = identityProvider.getSignInUrl
                    signInSessionCookie <- newSignInSession(state)
                    // TODO: Would be nice to be redirected to the current page instead of `/` after the login
                    raw                  = Response.redirect(signInUri).addCookie(signInSessionCookie)
                    response             = (sessionState match {
                                             case SessionState.NoSession => raw
                                             case SessionState.Expired   => raw.addCookie(userSessionInvalidationCookie)
                                           }): @nowarn("msg=match may not be exhaustive.")
                  } yield response
                }
              }
          }
        }
      }
    }

  private def getSessionContent(request: Request): UIO[SessionState] = {
    def invalidateSession(id: UUID): UIO[SessionState.Expired.type] =
      sessionStorage
        .invalidate(id)
        .foldZIO(
          success = _ => ZIO.succeed(SessionState.Expired),
          failure = e =>
            ZIO
              .logFatalCause("Failed to invalidate user session", Cause.fail(e))
              .as(SessionState.Expired),
        )

    ZIO.suspendSucceed {
      sessionId(request, userSessionCookieName) match {
        case None     => ZIO.succeed(SessionState.NoSession)
        case Some(id) =>
          sessionStorage
            .get(id)
            .foldZIO(
              failure = e =>
                ZIO
                  .logFatalCause("Failed to get the user session from the storage", Cause.fail(e))
                  .as(SessionState.Expired),
              success = {
                case None          =>
                  // The user sends a cookie with an ID but the storage doesn't contain this ID,
                  // we need to invalidate the cookie of the user
                  ZIO.succeed(SessionState.Expired)
                case Some(session) =>
                  session.content.fromJson[SessionCookieContent] match {
                    case Left(e)            =>
                      // TODO: There's a potential issue here as we might invalidate a "sign-in" session
                      ZIO.logError(s"Error while decoding the stored session cookie: $e") *>
                        invalidateSession(id)
                    case Right(sessionData) =>
                      if (session.notExpired(Instant.now())) ZIO.succeed(SessionState.Valid(sessionData))
                      else {
                        identityProvider
                          .refreshTokens(sessionData.refreshToken)
                          .foldZIO(
                            failure = e =>
                              ZIO.logErrorCause("Error while refreshing the tokens", Cause.fail(e)) *>
                                invalidateSession(id),
                            success = response =>
                              ZIO.suspendSucceed {
                                SessionCookieContent.from(response) match {
                                  case Right(newSession) =>
                                    sessionStorage
                                      .store(id, Session.make(newSession.toJson, response.expiresIn.seconds))
                                      .retry(Schedule.fixed(100.millis) && Schedule.recurs(3).unit)
                                      .foldZIO(
                                        success = _ => ZIO.succeed(SessionState.Valid(newSession)),
                                        failure = e =>
                                          ZIO
                                            .logFatalCause("Failed to store the user session in the session storage", Cause.fail(e))
                                            .as(SessionState.Expired),
                                      )
                                  case Left(e)           =>
                                    ZIO.logError(s"Error while decoding the new session cookie: $e") *>
                                      invalidateSession(id)
                                }
                              },
                          )
                      }
                  }
              },
            )
      }
    }
  }

  private def newSignInSession(state: String): Task[Cookie.Response] =
    (
      for {
        sessionId <- random.nextUUID
        expiresIn  = 1.hour
        _         <- sessionStorage
                       .store(sessionId, Session.make(state, expiresIn))
                       .retry(Schedule.fixed(100.millis) && Schedule.recurs(3).unit)
                       .tapErrorCause(e => ZIO.logFatalCause("Failed to store the sign-in session in the session storage", Cause.fail(e)))
      } yield Cookie
        .Response(
          name = signInCookieName,
          content = sessionId.toString,
          maxAge = Some(expiresIn), // This cookie is valid only 1h
          isSecure = false,         // TODO: should be `true` in Staging and Prod
          isHttpOnly = true,
          // Needs to be Lax otherwise the cookie isn't included in the `/auth/callback` request coming from Kinde
          sameSite = Some(Cookie.SameSite.Lax),
          path = Some(Path.root / "auth"),
        )
        .sign(cookieSignKey)
    ).logError("Error while creating the sign-in session")

  /**
   * That's not so important if the invalidation fails as the sign-in session only last 1 hours and as we'll remove the cookie containing the ID.
   */
  private def invalidateSignInSession(request: Request): UIO[Cookie.Response] =
    invalidateSession(request, signInCookieName).as(signInSessionInvalidationCookie)

  /**
   * That's not so important if the invalidation fails as we'll remove the cookie containing the ID.
   */
  private def invalidateUserSession(request: Request): UIO[Cookie.Response] =
    invalidateSession(request, userSessionCookieName).as(userSessionInvalidationCookie)

  private def invalidateSession(request: Request, cookieName: String): UIO[Unit] =
    ZIO.suspendSucceed {
      sessionId(request, cookieName) match {
        case None            => ZIO.unit
        case Some(sessionId) =>
          sessionStorage
            .invalidate(sessionId)
            .logError("Error while invalidating the session")
            .ignore
      }
    }

  private def sessionId(request: Request, cookieName: String): Option[UUID] =
    request
      .header(Header.Cookie)
      .flatMap(_.value.find(_.name == cookieName))
      .flatMap(_.unSign(cookieSignKey))
      .flatMap { cookie =>
        if (cookie.content.isBlank) None
        else
          try Some(UUID.fromString(cookie.content))
          catch {
            case NonFatal(_) => None
          }
      }

}
