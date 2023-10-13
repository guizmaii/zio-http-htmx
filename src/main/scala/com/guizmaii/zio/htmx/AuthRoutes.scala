package com.guizmaii.zio.htmx

import com.guizmaii.zio.htmx.services.{IdentityProvider, SessionManager}
import zio.ZIO
import zio.http.*

object AuthRoutes {

  /**
   * Based on https://docs.logto.io/docs/recipes/integrate-logto/traditional/
   */
  val routes: App[IdentityProvider & SessionManager] =
    Http.collectZIO[Request] {

      case Method.GET -> Root / "auth" / "sign-in" =>
        (
          for {
            sessionManager      <- ZIO.service[SessionManager]
            (signInUri, state)  <- ZIO.serviceWith[IdentityProvider](_.getSignInUrl)
            signInSessionCookie <- sessionManager.newSignInSession(state)
          } yield Response.redirect(signInUri).addCookie(signInSessionCookie)
        ).logError("Error while handling the sign-in request")
          .catchAll(_ => ZIO.succeed(Response.status(Status.InternalServerError))) // TODO Jules: Maybe not the thing to answer with htmx

      case req @ Method.GET -> Root / "auth" / "callback" =>
        (
          for {
            sessionManager  <- ZIO.service[SessionManager]
            maybeSavedState <- sessionManager.getSignInState(req)
            maybeState       = req.url.queryParams.get("state").flatMap(_.headOption.filter(_.nonEmpty))
            maybeCode        = req.url.queryParams.get("code").flatMap(_.headOption.filter(_.nonEmpty))
            response        <- (maybeCode, maybeSavedState, maybeState) match {
                                 case (Some(code), Some((signInSessionId, savedState)), Some(state)) if savedState == state =>
                                   (
                                     for {
                                       response                        <- ZIO.serviceWithZIO[IdentityProvider](_.handleSignIn(code))
                                       newSessionCookie                <- sessionManager.newSession(response)
                                       signInSessionInvalidationCookie <- sessionManager.invalidateSignInSession(signInSessionId)
                                     } yield
                                     // TODO Jules: For now, we always redirect to `/`, do we want/need to redirect somewhere else?
                                     Response
                                       .redirect(URL.root)
                                       .addCookie(newSessionCookie)
                                       .addCookie(signInSessionInvalidationCookie)
                                   )
                                     .logError("Error while handling the sign-in callback")
                                     .orElseFail(
                                       Response.status(Status.InternalServerError)
                                     ) // TODO Jules: Maybe not the thing to answer with htmx

                                 case _ =>
                                   ZIO.succeed(Response.status(Status.BadRequest))
                               }
          } yield response
        ).logError("Error while handling the sign-in callback")
          .catchAll(_ => ZIO.succeed(Response.status(Status.InternalServerError))) // TODO Jules: Maybe not the thing to answer with htmx

      case req @ Method.GET -> Root / "auth" / "sign-out" =>
        (
          for {
            sessionManager     <- ZIO.service[SessionManager]
            idp                <- ZIO.service[IdentityProvider]
            invalidationCookie <- sessionManager.invalidateSignInSession(req)
          } yield Response.redirect(idp.logoutUrl).addCookie(invalidationCookie)
        ).logError("Error while handling the sign-out request")
          .catchAll(_ => ZIO.succeed(Response.status(Status.InternalServerError))) // TODO Jules: Maybe not the thing to answer with htmx
    }

}
