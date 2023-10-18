package com.guizmaii.zio.htmx.services

import com.guizmaii.zio.htmx.domain.LoggedUser
import com.guizmaii.zio.htmx.persistence.{Session, SessionIdEncoder, SessionStorage}
import com.guizmaii.zio.htmx.services.SessionCookieType.{SignInSessionCookie, UserSessionCookie}
import com.guizmaii.zio.htmx.types.CookieSignKey
import com.guizmaii.zio.htmx.utils.ShouldNeverHappen
import com.guizmaii.zio.htmx.{AppConfig, RuntimeEnv}
import zio.http.*
import zio.json.{DecoderOps, DeriveJsonCodec, JsonCodec}
import zio.prelude.ForEachOps
import zio.{Cause, Duration, Schedule, Task, UIO, URLayer, ZIO, ZLayer, durationInt, durationLong}

import java.time.Instant
import scala.annotation.nowarn
import scala.util.control.NonFatal

final case class SignInSessionContent(state: String, redirectTo: URL)
object SignInSessionContent {
  implicit private[SignInSessionContent] val urlCodec: JsonCodec[URL] =
    JsonCodec.string
      .transform[URL](
        s => URL.decode(s).fold(e => throw ShouldNeverHappen("Failed to decode URL", e), identity),
        _.encode,
      )

  implicit val codec: JsonCodec[SignInSessionContent] = DeriveJsonCodec.gen[SignInSessionContent]
}

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

sealed trait SessionState {
  def asSome[A](f: UserSessionContent => A): Option[A] =
    this match {
      case SessionState.NoSession      => None
      case SessionState.Expired        => None
      case SessionState.Valid(session) => Some(f(session))
    }
}
object SessionState       {
  case object NoSession                               extends SessionState
  case object Expired                                 extends SessionState
  final case class Valid(session: UserSessionContent) extends SessionState
}

sealed trait SessionId {
  def encode(implicit encoder: SessionIdEncoder[SessionId]): String = encoder.encode(this)
}
object SessionId       {
  final case class SignInSessionId(id: String) extends SessionId
  final case class UserSessionId(id: String)   extends SessionId

  val nextSignInSessionId: UIO[SignInSessionId] = ZIO.random.flatMap(_.nextUUID).map(uuid => SignInSessionId(s"s-$uuid"))
  val nextUserSessionId: UIO[UserSessionId]     = ZIO.random.flatMap(_.nextUUID).map(uuid => UserSessionId(s"u-$uuid"))

  implicit val encoder: SessionIdEncoder[SessionId] = {
    case SessionId.SignInSessionId(id) => id
    case SessionId.UserSessionId(id)   => id
  }
}

sealed trait SessionCookieType {
  type SessionId <: com.guizmaii.zio.htmx.services.SessionId

  def name: String
  def makeSessionId(string: String): SessionId
}
object SessionCookieType       {
  case object SignInSessionCookie extends SessionCookieType {
    override type SessionId = SessionId.SignInSessionId

    override def name: String                                             = "HTMX_SIGN_IN"
    override def makeSessionId(string: String): SessionId.SignInSessionId = SessionId.SignInSessionId.apply(string)
  }
  case object UserSessionCookie   extends SessionCookieType {
    override type SessionId = SessionId.UserSessionId

    override def name: String                                           = "HTMX_SESSION"
    override def makeSessionId(string: String): SessionId.UserSessionId = SessionId.UserSessionId.apply(string)
  }
}

//noinspection NonAsciiCharacters
trait SessionManager {
  def signInUrl(redirectTo: URL): Task[(URL, Cookie.Response)]
  def logoutUrl(request: Request): UIO[(URL, Cookie.Response, Cookie.Response)]
  def newUserSession(request: Request): Task[(URL, Cookie.Response, Cookie.Response)]
  def loggedUser(request: Request): UIO[Option[LoggedUser]]

  def authMiddleware: HandlerAspect[Any, LoggedUser]
}

object SessionManager {
  def live: URLayer[RuntimeEnv & SessionStorage[SessionId] & IdentityProvider & AppConfig, SessionManagerLive] =
    ZLayer.fromZIO {
      for {
        appConfig      <- ZIO.service[AppConfig]
        idp            <- ZIO.service[IdentityProvider]
        sessionStorage <- ZIO.service[SessionStorage[SessionId]]
        runtimeEnv     <- ZIO.service[RuntimeEnv]
      } yield new SessionManagerLive(appConfig.cookieSignKey, idp, sessionStorage, runtimeEnv)
    }
}

//noinspection NonAsciiCharacters
final class SessionManagerLive(
  cookieSignKey: CookieSignKey,
  identityProvider: IdentityProvider,
  sessionStorage: SessionStorage[SessionId],
  runtimeEnv: RuntimeEnv,
) extends SessionManager {
  private val signInSessionInvalidationCookie: Cookie.Response =
    Cookie
      .Response(
        name = SignInSessionCookie.name,
        content = "",
        maxAge = Some(Duration.Zero), // expires the session cookie
        isSecure = runtimeEnv.isProdLike,
        isHttpOnly = true,
        sameSite = Some(Cookie.SameSite.Lax),
        path = Some(Path.root / "auth"),
      )
      .sign(cookieSignKey)

  private val userSessionInvalidationCookie: Cookie.Response =
    Cookie
      .Response(
        name = UserSessionCookie.name,
        content = "",
        maxAge = Some(Duration.Zero), // expires the session cookie
        isSecure = runtimeEnv.isProdLike,
        isHttpOnly = true,
        sameSite = Some(Cookie.SameSite.Lax),
        path = Some(Path.root),
      )
      .sign(cookieSignKey)

  override def signInUrl(redirectTo: URL): Task[(URL, Cookie.Response)] =
    (
      for {
        (signInUrl, state)  <- ZIO.succeed(identityProvider.getSignInUrl)
        signInSessionCookie <- newSignInSession(state, redirectTo = redirectTo)
      } yield (signInUrl, signInSessionCookie)
    ).logError("Error while creating the sign-in session")

  override def logoutUrl(request: Request): UIO[(URL, Cookie.Response, Cookie.Response)] =
    for {
      signInSessionInvalidationCookie <- invalidateSignInSession(request)
      userSessionInvalidationCookie   <- invalidateUserSession(request)
    } yield (identityProvider.logoutUrl, signInSessionInvalidationCookie, userSessionInvalidationCookie)

  override def newUserSession(request: Request): Task[(URL, Cookie.Response, Cookie.Response)] = {
    def signInState(request: Request): UIO[Option[SignInSessionContent]] =
      ZIO
        .suspendSucceed {
          sessionId(request, SignInSessionCookie) match {
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
                  success = _.map(s => s.decode[SignInSessionContent] -> s) match {
                    case None                                                                              => ZIO.none
                    case Some((Right(signInSessionContent), session)) if session.notExpired(Instant.now()) => ZIO.some(signInSessionContent)
                    case _                                                                                 =>
                      // We can ignore here as the session cookie will be wiped out anyway
                      sessionStorage
                        .invalidate(id)
                        .retry(Schedule.fixed(100.millis) && Schedule.recurs(3).unit)
                        .ignore
                        .as(None)
                  },
                )
          }
        }

    def newSession(codeTokenResponse: CodeTokenResponse): Task[Cookie.Response] =
      (
        for {
          sessionId  <- SessionId.nextUserSessionId
          expiresIn   = codeTokenResponse.expiresIn.seconds
          userSession = Session.make(UserSessionContent.from(codeTokenResponse), expiresIn)
          _          <- sessionStorage
                          .store(sessionId, userSession)
                          .retry(Schedule.fixed(100.millis) && Schedule.recurs(3).unit)
                          .tapErrorCause(e => ZIO.logFatalCause("Failed to store the user session in the session storage", Cause.fail(e)))
        } yield Cookie
          .Response(
            name = UserSessionCookie.name,
            content = sessionId.encode,
            maxAge = Some(expiresIn),
            isSecure = runtimeEnv.isProdLike,
            isHttpOnly = true,
            // Needs to be Lax otherwise the cookie isn't included in `/` request coming from Kinde after login
            sameSite = Some(Cookie.SameSite.Lax),
            path = Some(Path.root),
          )
          .sign(cookieSignKey)
      ).logError("Error while creating the session")

    for {
      maybeSignInSession <- signInState(request)
      maybeState          = request.url.queryParams.get("state").filter(_.nonEmpty)
      maybeCode           = request.url.queryParams.get("code").filter(_.nonEmpty)
      cookies            <- (maybeCode, maybeSignInSession, maybeState) match {
                              case (Some(code), Some(SignInSessionContent(savedState, redirectTo)), Some(state)) if savedState == state =>
                                for {
                                  tokens                          <- identityProvider.handleSignIn(code)
                                  newSessionCookie                <- newSession(tokens)
                                  signInSessionInvalidationCookie <- invalidateSignInSession(request)
                                } yield (redirectTo, newSessionCookie, signInSessionInvalidationCookie)
                              case _                                                                                                    =>
                                ZIO.fail(new RuntimeException("Invalid state"))
                            }
    } yield cookies
  }

  override def loggedUser(request: Request): UIO[Option[LoggedUser]] =
    userSessionState(request).map(_.asSome(_.loggedUser))

  override val authMiddleware: HandlerAspect[Any, LoggedUser] =
    Middleware.interceptIncomingHandler {
      Handler.fromFunctionZIO { request =>
        userSessionState(request)
          .flatMap {
            case SessionState.Valid(session)                   => ZIO.succeed((request, session.loggedUser))
            case SessionState.NoSession | SessionState.Expired =>
              ZIO.suspendSucceed {
                val (signInUri, state) = identityProvider.getSignInUrl

                newSignInSession(state, redirectTo = request.url)
                  .foldZIO(
                    failure = e =>
                      ZIO.logFatalCause("Error while handling the sign-in request", Cause.fail(e)) *>
                        ZIO.fail(Response.status(Status.InternalServerError)), // TODO Jules: Maybe not the thing to answer with htmx
                    success = signInSessionCookie =>
                      ZIO.fail {
                        Response
                          .redirect(signInUri)
                          .addCookie(signInSessionCookie)
                          .addCookie(userSessionInvalidationCookie) // We always invalidate the user session cookie to be secure.
                      },
                  )
              }
          }
      }
    }

  private def userSessionState(request: Request): UIO[SessionState] = {
    def invalidateSession(id: SessionId): UIO[SessionState.Expired.type] =
      sessionStorage
        .invalidate(id)
        .retry(Schedule.fixed(100.millis) && Schedule.recurs(3).unit)
        .foldZIO(
          success = _ => ZIO.succeed(SessionState.Expired),
          failure = e =>
            ZIO
              .logFatalCause("Failed to invalidate user session", Cause.fail(e))
              .as(SessionState.Expired),
        )

    ZIO.suspendSucceed {
      sessionId(request, UserSessionCookie) match {
        case None     => ZIO.succeed(SessionState.NoSession)
        case Some(id) =>
          sessionStorage
            .get(id)
            .retry(Schedule.fixed(100.millis) && Schedule.recurs(3).unit)
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
                    case Left(e)                   =>
                      ZIO.logError(s"Error while decoding the stored session cookie: $e") *>
                        invalidateSession(id)
                    case Right(userSessionContent) =>
                      if (session.notExpired(Instant.now())) ZIO.succeed(SessionState.Valid(userSessionContent))
                      else {
                        identityProvider
                          .refreshTokens(userSessionContent.refreshToken)
                          .foldZIO(
                            failure = e =>
                              ZIO.logErrorCause("Error while refreshing the tokens", Cause.fail(e)) *>
                                invalidateSession(id),
                            success = tokens =>
                              ZIO.suspendSucceed {
                                val newUserSessionContent = UserSessionContent.from(tokens)
                                val newUserSession        = Session.make(newUserSessionContent, tokens.expiresIn.seconds)
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

  private def newSignInSession(state: String, redirectTo: URL): Task[Cookie.Response] =
    (
      for {
        sessionId    <- SessionId.nextSignInSessionId
        expiresIn     = 1.hour
        signInSession = Session.make(SignInSessionContent(state = state, redirectTo = redirectTo), expiresIn)
        _            <- sessionStorage
                          .store(sessionId, signInSession)
                          .retry(Schedule.fixed(100.millis) && Schedule.recurs(3).unit)
                          .tapErrorCause(e => ZIO.logFatalCause("Failed to store the sign-in session in the session storage", Cause.fail(e)))
      } yield Cookie
        .Response(
          name = SignInSessionCookie.name,
          content = sessionId.encode,
          maxAge = Some(expiresIn), // This cookie is valid only 1h
          isSecure = runtimeEnv.isProdLike,
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
    invalidateSession(request, SignInSessionCookie).as(signInSessionInvalidationCookie)

  /**
   * That's not so important if the invalidation fails as we'll remove the cookie containing the ID.
   */
  private def invalidateUserSession(request: Request): UIO[Cookie.Response] =
    invalidateSession(request, UserSessionCookie).as(userSessionInvalidationCookie)

  private def invalidateSession(request: Request, cookieName: SessionCookieType): UIO[Unit] =
    ZIO.suspendSucceed {
      sessionId(request, cookieName) match {
        case None            => ZIO.unit
        case Some(sessionId) =>
          sessionStorage
            .invalidate(sessionId)
            .retry(Schedule.fixed(100.millis) && Schedule.recurs(3).unit)
            .logError("Error while invalidating the session")
            .ignore
      }
    }

  private def sessionId(request: Request, sessionCookieType: SessionCookieType): Option[sessionCookieType.SessionId] =
    request
      .header(Header.Cookie)
      .flatMap(_.value.find(_.name == sessionCookieType.name))
      .flatMap(_.unSign(cookieSignKey))
      .flatMap { cookie =>
        if (cookie.content.isBlank) None
        else
          try Some(sessionCookieType.makeSessionId(cookie.content))
          catch {
            case NonFatal(_) => None
          }
      }

}
