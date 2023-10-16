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

      case req @ Method.GET -> Root / "auth" / "sign-in" =>
        for {
          sessionManager  <- ZIO.service[SessionManager]
          redirectTo       = req.url.queryParams
                               .get("redirectTo")
                               .flatMap(_.headOption)
                               .filter(_.nonEmpty)
                               .flatMap(URL.decode(_).toOption)
                               .getOrElse(URL.root)
          maybeLoggedUser <- sessionManager.loggedUser(req)
          response        <- maybeLoggedUser match {
                               case Some(_) => ZIO.succeed(Response.redirect(redirectTo))
                               case None    =>
                                 sessionManager
                                   .signInUrl(redirectTo)
                                   .foldZIO(
                                     failure = e =>
                                       ZIO
                                         .logErrorCause("Error while handling the sign-in request", Cause.fail(e))
                                         .as(Response.status(Status.InternalServerError)), // TODO Jules: Maybe not the thing to answer with htmx
                                     success = { case (signInUrl, signInSession) =>
                                       ZIO.succeed(Response.redirect(signInUrl).addCookie(signInSession))
                                     },
                                   )
                             }
        } yield response

      case req @ Method.GET -> Root / "auth" / "callback" =>
        ZIO
          .serviceWithZIO[SessionManager](_.newUserSession(req))
          .foldZIO(
            failure = e =>
              ZIO
                .logErrorCause("Error while handling the sign-in callback", Cause.fail(e))
                .as(Response.status(Status.Unauthorized)), // TODO Jules: Can we do better?
            success = { case (redirectTo, newUserSession, signInSessionInvalidation) =>
              ZIO.succeed {
                Response
                  .redirect(redirectTo)
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
