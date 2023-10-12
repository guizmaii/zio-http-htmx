package com.guizmaii.zio.htmx

import com.guizmaii.zio.htmx.services.{IdentityProvider, SessionCookieContent, SessionManager}
import zio.http.*
import zio.{ZIO, durationLong}

import java.time.Instant

object AuthRoutes {

  /**
   * Based on https://docs.logto.io/docs/recipes/integrate-logto/traditional/
   */
  val routes: App[IdentityProvider & SessionManager] =
    Http.collectZIO[Request] {

      case Method.GET -> Root / "auth" / "sign-in" =>
        for {
          sessionManager     <- ZIO.service[SessionManager]
          (signInUri, state) <- ZIO.serviceWith[IdentityProvider](_.getSignInUrl)
        } yield Response.redirect(signInUri).addCookie(sessionManager.signInCookie(state))

      case req @ Method.GET -> Root / "auth" / "callback" =>
        for {
          sessionManager <- ZIO.service[SessionManager]
          maybeSavedState = sessionManager.getSignInState(req)
          maybeState      = req.url.queryParams.get("state").flatMap(_.headOption.filter(_.nonEmpty))
          maybeCode       = req.url.queryParams.get("code").flatMap(_.headOption.filter(_.nonEmpty))
          response       <- (maybeCode, maybeSavedState, maybeState) match {
                              case (Some(code), Some(savedState), Some(state)) if savedState == state =>
                                (
                                  for {
                                    response  <- ZIO.serviceWithZIO[IdentityProvider](_.handleSignIn(code))
                                    newSession = SessionCookieContent(
                                                   expiresAt = Instant.now().plusSeconds(response.expiresIn),
                                                   refreshToken = response.refreshToken,
                                                   idToken = response.idToken,
                                                 )
                                  } yield
                                  // TODO Jules: For now, we always redirect to `/`, do we want/need to redirect somewhere else?
                                  Response
                                    .redirect(URL.root)
                                    .addCookie(sessionManager.newSessionCookie(newSession, response.expiresIn.seconds))
                                    .addCookie(sessionManager.signInInvalidationCookie)
                                )
                                  .logError("Error while handling the sign-in callback")
                                  .orElseFail(Response.status(Status.InternalServerError)) // TODO Jules: Maybe not the thing to answer with htmx

                              case _ =>
                                ZIO.succeed(Response.status(Status.BadRequest))
                            }
        } yield response

      case Method.GET -> Root / "auth" / "sign-out" =>
        for {
          sessionManager <- ZIO.service[SessionManager]
          idp            <- ZIO.service[IdentityProvider]
        } yield Response.redirect(idp.logoutUrl).addCookie(sessionManager.sessionInvalidationCookie)
    }

}
