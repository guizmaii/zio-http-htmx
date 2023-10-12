package com.guizmaii.zio.htmx

import com.guizmaii.zio.htmx.services.IdentityProvider
import com.guizmaii.zio.htmx.types.CookieSignKey
import zio.http.*
import zio.json.{DecoderOps, DeriveJsonCodec, EncoderOps, JsonCodec}
import zio.{Cause, Duration, Trace, ZEnvironment, ZIO, durationLong}

import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.Base64
import scala.annotation.nowarn
import scala.util.control.NonFatal

//noinspection NonAsciiCharacters
object AuthRoutes {

  private val sessionCookieName = "HTMX_SESSION"

  private def sessionInvalidationCookie(signKey: CookieSignKey): Cookie.Response =
    Cookie
      .Response(
        name = sessionCookieName,
        content = "",
        maxAge = Some(Duration.Zero), // expires the session cookie
        isSecure = false,             // TODO: should be `true` in Staging and Prod
        isHttpOnly = true,
        sameSite = Some(Cookie.SameSite.Strict),
        path = Some(Path.root),
      )
      .sign(signKey)

  private def newSessionCookie(signKey: CookieSignKey, newSession: SessionCookieContent, expiresIn: Long): Cookie.Response =
    Cookie
      .Response(
        name = sessionCookieName,
        content = SessionCookieContent.encode(newSession),
        maxAge = Some(expiresIn.seconds),
        isSecure = false, // TODO: should be `true` in Staging and Prod
        isHttpOnly = true,
        sameSite = Some(Cookie.SameSite.Strict),
        path = Some(Path.root),
      )
      .sign(signKey)

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
  val routes: App[IdentityProvider & AppConfig] =
    Http.collectZIO[Request] {

      case Method.GET -> Root / "auth" / "sign-in" =>
        for {
          signInUri <- ZIO.serviceWith[IdentityProvider](_.getSignInUrl)
        } yield Response.redirect(signInUri)

      case req @ Method.GET -> Root / "auth" / "callback" =>
        (
          ZIO
            .serviceWithZIO[AppConfig] { appConfig =>
              // TODO Jules: Validate state
              req.url.queryParams.get("code").flatMap(_.headOption.filter(_.nonEmpty)) match {
                case None       => ZIO.succeed(Response.status(Status.BadRequest))
                case Some(code) =>
                  for {
                    response  <- ZIO.serviceWithZIO[IdentityProvider](_.handleSignIn(code))
                    newSession = SessionCookieContent(
                                   expiresAt = Instant.now().plusSeconds(response.expiresIn),
                                   refreshToken = response.refreshToken,
                                   idToken = response.idToken,
                                 )
                  } yield Response.redirect(URL.root).addCookie(newSessionCookie(appConfig.cookieSignKey, newSession, response.expiresIn))
              }
            }
          )
          .logError("Error while handling the sign-in callback")
          .catchAll(_ => ZIO.succeed(Response.status(Status.InternalServerError)))

      case Method.GET -> Root / "auth" / "sign-out" =>
        ZIO.serviceWith[AppConfig] { config =>
          Response.redirect(URL.root).addCookie(sessionInvalidationCookie(config.cookieSignKey))
        }
    }

  private def sessionCookieMiddleware[Context: zio.Tag](
    appConfig: AppConfig,
    kinde: IdentityProvider, // TODO: Didn't found how to pass it by the env 🤔
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
              .flatMap(_.value.find(_.name == sessionCookieName))
              .map(
                _.unSign(appConfig.cookieSignKey)
                  .toRight("Invalid cookie signature")
                  .flatMap(cookie => SessionCookieContent.decode(cookie.content))
              ) match {

              case None => ZIO.succeed(Http.fromHandler(Handler.forbidden("Missing session cookie")))

              case Some(Left(e)) =>
                ZIO
                  .logError(s"Invalid session cookie: $e")
                  .as(
                    Http.fromHandler(
                      Handler.response(
                        Response.status(Status.Unauthorized).addCookie(sessionInvalidationCookie(appConfig.cookieSignKey))
                      )
                    )
                  )

              case Some(Right(session)) =>
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
                              Response.status(Status.Unauthorized).addCookie(sessionInvalidationCookie(appConfig.cookieSignKey))
                            )
                          )
                        )
                  }
                } else {
                  (
                    for {
                      response  <- kinde.refreshTokens(session.refreshToken)
                      newSession = SessionCookieContent(
                                     expiresAt = Instant.now().plusSeconds(response.expiresIn),
                                     refreshToken = response.refreshToken,
                                     idToken = response.idToken,
                                   )
                      r         <- extractContext(newSession) match {
                                     case Right(context) =>
                                       ZIO.succeed {
                                         providedHttp(context).map(
                                           _.addCookie(newSessionCookie(appConfig.cookieSignKey, newSession, response.expiresIn))
                                         )
                                       }
                                     case Left(error)    =>
                                       ZIO.logErrorCause("Error while extracting the context from the session cookie", Cause.fail(error)) *>
                                         ZIO.succeed {
                                           Http.fromHandler(
                                             Handler.response(
                                               // TODO: Is `Unauthorized` appropriate here?
                                               // TODO: Should we really wipe the session cookie here?
                                               Response.status(Status.Unauthorized).addCookie(sessionInvalidationCookie(appConfig.cookieSignKey))
                                             )
                                           )
                                         }
                                   }
                    } yield r
                  ).logError("Error while refreshing the tokens")
                    .catchAll(_ =>
                      ZIO.succeed(
                        Http.fromHandler(
                          Handler.response(
                            // TODO: /sign-in URL isn't the correct one here
                            Response.redirect(URL.root.withPath("/sign-in")).addCookie(sessionInvalidationCookie(appConfig.cookieSignKey))
                          )
                        )
                      )
                    )
                }
            }
          }
        }
      }
    }

  final case class User(email: String)

  // TODO Jules: https://kinde.com/docs/build/about-id-tokens/
  final case class IdToken(email: String)
  object IdToken {
    // TODO Jules
    @nowarn
    def decode(s: String): IdToken = IdToken(email = "toto@example.com")
  }

  def authMiddleware(
    appConfig: AppConfig,
    logToService: IdentityProvider,
  ): HttpAppMiddleware.WithOut[Nothing, Any, Nothing, Throwable, λ[Env => Env & User], λ[A => Throwable]] =
    sessionCookieMiddleware[User](
      appConfig,
      logToService,
      session =>
        try {
          val idToken = IdToken.decode(session.idToken)
          Right(User(email = idToken.email))
        } catch {
          case NonFatal(e) => Left(e)
        },
    )

}
