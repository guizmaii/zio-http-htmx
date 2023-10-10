package com.guizmaii.zio.htmx

import com.guizmaii.zio.htmx.services.{LogToService, SignInSession}
import io.logto.sdk.core.util.TokenUtils
import zio.http.*
import zio.json.{DecoderOps, DeriveJsonCodec, EncoderOps, JsonCodec}
import zio.{Cause, Duration, Trace, ZEnvironment, ZIO, durationLong}

import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.Base64
import scala.util.control.NonFatal

//noinspection NonAsciiCharacters
object AuthRoutes {

  private val cookieSignInName = "HTMX_SIGN_IN"
  private val cookieTokensName = "HTMX_SESSION"

  final case class SessionCookieContent(expiresAt: Instant, refreshToken: String, idToken: String)
  object SessionCookieContent {
    implicit private val codec: JsonCodec[SessionCookieContent] = DeriveJsonCodec.gen[SessionCookieContent]

    def encode(sessionCookieContent: SessionCookieContent): String =
      Base64.getUrlEncoder.encodeToString(sessionCookieContent.toJson.getBytes(StandardCharsets.UTF_8))

    def decode(s: String): Either[String, SessionCookieContent] =
      try new String(Base64.getUrlDecoder.decode(s), StandardCharsets.UTF_8).fromJson[SessionCookieContent]
      catch {
        case NonFatal(_) => Left("Invalid cookie encoding")
      }
  }

  /**
   * Based on https://docs.logto.io/docs/recipes/integrate-logto/traditional/
   */
  val routes: App[LogToService & AppConfig] =
    Http.collectZIO[Request] {

      case Method.GET -> Root / "sign-in" =>
        (
          for {
            config                     <- ZIO.service[AppConfig]
            (signInSession, signInUri) <- ZIO.serviceWithZIO[LogToService](_.getSignInUrl)
            cookie                      = Cookie
                                            .Response(
                                              name = cookieSignInName,
                                              content = SignInSession.encode(signInSession),
                                              maxAge = Some(1.hour), // TODO Jules: Is this correct?
                                              isHttpOnly = true,
                                            )
                                            .sign(config.cookieSignKey)
          } yield Response.redirect(signInUri).addCookie(cookie)
        ).logError("Error while getting the sign-in URL")
          .catchAll(_ => ZIO.succeed(Response.status(Status.InternalServerError)))

      case req @ Method.GET -> Root / "callback" =>
        (
          ZIO
            .serviceWithZIO[AppConfig] { appConfig =>
              req
                .header(Header.Cookie)
                .flatMap(_.value.find(_.name == cookieSignInName))
                .map(_.unSign(appConfig.cookieSignKey).toRight("Invalid cookie signature"))
                .map(_.flatMap(cookie => SignInSession.decode(cookie.content))) match {

                case None => ZIO.succeed(Response.status(Status.BadRequest))

                case Some(Left(e)) =>
                  ZIO.logError(s"Failed decoding the cookie content: $e") *>
                    ZIO.succeed(Response.status(Status.BadRequest))

                case Some(Right(session)) =>
                  for {
                    config      <- ZIO.service[AppConfig]
                    url          = s"http://localhost:8080${req.url.encode}"
                    response    <- ZIO.serviceWithZIO[LogToService](_.handleSignIn(session, url))
                    newSession   = SessionCookieContent(
                                     expiresAt = Instant.now().plusSeconds(response.getExpiresIn),
                                     refreshToken = response.getRefreshToken,
                                     idToken = response.getIdToken,
                                   )
                    tokensCookie = Cookie
                                     .Response(
                                       name = cookieTokensName,
                                       content = SessionCookieContent.encode(newSession),
                                       maxAge = Some(response.getExpiresIn.seconds),
                                       isHttpOnly = true,
                                     )
                                     .sign(config.cookieSignKey)
                    signInCookie = Cookie
                                     .Response(
                                       name = cookieSignInName,
                                       content = "",
                                       maxAge = Some(Duration.Zero),
                                       isHttpOnly = true,
                                     )
                                     .sign(config.cookieSignKey)
                  } yield Response.redirect(URL.root).addCookie(tokensCookie).addCookie(signInCookie)
              }
            }
          )
          .logError("Error while handling the sign-in callback")
          .catchAll(_ => ZIO.succeed(Response.status(Status.InternalServerError)))

      case Method.GET -> Root / "sign-out" =>
        ZIO.serviceWith[AppConfig] { config =>
          val tokensCookie =
            Cookie
              .Response(
                name = cookieTokensName,
                content = "",
                maxAge = Some(Duration.Zero), // TODO Jules: Does it remove the cookie from the Browser?
                isHttpOnly = true,
              )
              .sign(config.cookieSignKey)

          Response.redirect(URL.root).addCookie(tokensCookie)
        }
    }

  private def sessionCookieMiddleware[Context: zio.Tag](
    appConfig: AppConfig,
    logToService: LogToService, // TODO: Didn't found how to pass it by the env 🤔
    extractContext: SessionCookieContent => Either[Throwable, Context],
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
            request
              .header(Header.Cookie)
              .flatMap(_.value.find(_.name == cookieTokensName))
              .map(_.unSign(appConfig.cookieSignKey).toRight("Invalid cookie signature"))
              .map(_.flatMap(cookie => SessionCookieContent.decode(cookie.content))) match {

              case None => ZIO.succeed(Http.fromHandler(Handler.forbidden("Missing session cookie")))

              case Some(Left(e)) =>
                ZIO
                  .logError(s"Invalid session cookie: $e")
                  .as(Http.fromHandler(Handler.forbidden("Invalid session cookie")))

              case Some(Right(session)) =>
                val isSessionStillValid = Instant.now().isBefore(session.expiresAt)

                if (isSessionStillValid)
                  extractContext(session) match {
                    case Right(context) => ZIO.succeed(providedHttp(context))
                    case Left(error)    =>
                      ZIO.logErrorCause("Error while extracting the context from the session cookie", Cause.fail(error)) *>
                        ZIO.succeed(Http.fromHandler(Handler.forbidden("Invalid session cookie")))
                  }
                else {
                  (
                    for {
                      response       <- logToService.refreshTokens(session.refreshToken)
                      newSession      = SessionCookieContent(
                                          expiresAt = Instant.now().plusSeconds(response.getExpiresIn),
                                          refreshToken = response.getRefreshToken,
                                          idToken = response.getIdToken,
                                        )
                      refreshedCookie = Cookie.Response(
                                          name = cookieTokensName,
                                          content = SessionCookieContent.encode(newSession),
                                          maxAge = Some(response.getExpiresIn.seconds),
                                          isHttpOnly = true,
                                        )
                      r              <- extractContext(newSession) match {
                                          case Right(context) => ZIO.succeed(providedHttp(context).map(_.addCookie(refreshedCookie)))
                                          case Left(error)    =>
                                            ZIO.logErrorCause("Error while extracting the context from the session cookie", Cause.fail(error)) *>
                                              ZIO.succeed(Http.fromHandler(Handler.forbidden("Invalid session cookie")))
                                        }
                    } yield r
                  ).logError("Error while refreshing the tokens")
                    .catchAll(_ => ZIO.succeed(Http.fromHandler(Handler.response(Response.redirect(URL.root.withPath("/sign-in"))))))
                }
            }
          }
        }
      }
    }

  final case class User(email: String)

  def authMiddleware(
    appConfig: AppConfig,
    logToService: LogToService,
  ): HttpAppMiddleware.WithOut[Nothing, Any, Nothing, Throwable, λ[Env => Env & User], λ[A => Throwable]] =
    sessionCookieMiddleware[User](
      appConfig,
      logToService,
      session =>
        try {
          val idToken = TokenUtils.INSTANCE.decodeIdToken(session.idToken)
          Right(User(email = idToken.getEmail))
        } catch {
          case NonFatal(e) => Left(e)
        },
    )

}
