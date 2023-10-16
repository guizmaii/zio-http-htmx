package com.guizmaii.zio.htmx.services

import com.guizmaii.zio.htmx.AppConfig
import com.guizmaii.zio.htmx.domain.LoggedUser
import com.guizmaii.zio.htmx.persistence.{Session, SessionStorage}
import com.guizmaii.zio.htmx.types.CookieSignKey
import zio.http.*
import zio.json.{DecoderOps, DeriveJsonCodec, JsonCodec}
import zio.prelude.ForEachOps
import zio.{Cause, Duration, Random, Schedule, Task, Trace, UIO, URLayer, ZEnvironment, ZIO, ZLayer, durationInt, durationLong}

import java.time.Instant
import java.util.UUID
import scala.annotation.nowarn
import scala.util.control.NonFatal

@nowarn("cat=scala3-migration")
final case class UserSessionContent private (refreshToken: String, loggedUser: LoggedUser)
object UserSessionContent {
  implicit val codec: JsonCodec[UserSessionContent] = DeriveJsonCodec.gen[UserSessionContent]

  private[services] def from(codeTokenResponse: CodeTokenResponse): UserSessionContent =
    UserSessionContent(
      refreshToken = codeTokenResponse.refreshToken,
      loggedUser = LoggedUser.from(codeTokenResponse.idToken),
    )
}

sealed trait SessionState extends Product with Serializable {
  def asSome[A](f: UserSessionContent => A): Option[A] =
    this match {
      case SessionState.NoSession      => None
      case SessionState.Expired        => None
      case SessionState.Valid(session) => Some(f(session))
    }
}
object SessionState {
  case object NoSession                               extends SessionState
  case object Expired                                 extends SessionState
  final case class Valid(session: UserSessionContent) extends SessionState
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
          sessionId  <- random.nextUUID
          expiresIn   = codeTokenResponse.expiresIn.seconds
          userSession = Session.make(UserSessionContent.from(codeTokenResponse), expiresIn)
          _          <- sessionStorage
                          .store(sessionId, userSession)
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
                               tokens                          <- identityProvider.handleSignIn(code)
                               newSessionCookie                <- newSession(tokens)
                               signInSessionInvalidationCookie <- invalidateSignInSession(request)
                             } yield (newSessionCookie, signInSessionInvalidationCookie)
                           case _                                                                  =>
                             ZIO.fail(new RuntimeException("Invalid state")) // TODO Jules: can we do better?
                         }
    } yield cookies
  }

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
                    (signInUri, state)  <- ZIO.succeed(identityProvider.getSignInUrl)
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
                  session.content.fromJson[UserSessionContent] match {
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
                                val newUserSessionContent = UserSessionContent.from(response)
                                val newUserSession        = Session.make(newUserSessionContent, response.expiresIn.seconds)
                                sessionStorage
                                  .store(id, newUserSession)
                                  .retry(Schedule.fixed(100.millis) && Schedule.recurs(3).unit)
                                  .foldZIO(
                                    success = _ => ZIO.succeed(SessionState.Valid(newUserSessionContent)),
                                    failure = e =>
                                      ZIO
                                        .logFatalCause("Failed to store the user session in the session storage", Cause.fail(e))
                                        .as(SessionState.Expired),
                                  )
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
