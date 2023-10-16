package com.guizmaii.zio.htmx

import com.guizmaii.zio.htmx.services.SessionManager
import zio.http.*
import zio.{Cause, ZIO}

object AuthRoutes {

  /**
   * Based on https://docs.logto.io/docs/recipes/integrate-logto/traditional/
   */
  val routes: App[SessionManager] =
    Http.collectZIO[Request] {

      case Method.GET -> Root / "auth" / "sign-in" =>
        ZIO
          .serviceWithZIO[SessionManager](_.signInUrl)
          .foldZIO(
            failure = e =>
              ZIO
                .logErrorCause("Error while handling the sign-in request", Cause.fail(e))
                .as(Response.status(Status.InternalServerError)), // TODO Jules: Maybe not the thing to answer with htmx
            success = { case (signInUrl, signInSession) =>
              ZIO.succeed(Response.redirect(signInUrl).addCookie(signInSession))
            },
          )

      case req @ Method.GET -> Root / "auth" / "callback" =>
        ZIO
          .serviceWithZIO[SessionManager](_.newUserSession(req))
          .foldZIO(
            failure = e =>
              ZIO
                .logErrorCause("Error while handling the sign-in callback", Cause.fail(e))
                .as(Response.status(Status.Unauthorized)), // TODO Jules: Can we do better?
            success = { case (newUserSession, signInSessionInvalidation) =>
              ZIO.succeed {
                // TODO Jules: For now, we always redirect to `/`, do we want/need to redirect somewhere else?
                Response
                  .redirect(URL.root)
                  .addCookie(newUserSession)
                  .addCookie(signInSessionInvalidation)
              }
            },
          )

      case req @ Method.GET -> Root / "auth" / "sign-out" =>
        ZIO
          .serviceWithZIO[SessionManager](_.logoutUrl(req))
          .map { case (url, userSessionInvalidation, signInSessionInvalidation) =>
            Response
              .redirect(url)
              .addCookie(userSessionInvalidation)
              .addCookie(signInSessionInvalidation) // We also invalidate the "sign-in" cookie, just in case.
          }
    }

}
