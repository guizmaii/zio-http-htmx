package com.guizmaii.zio.htmx

import com.guizmaii.zio.htmx.domain.LoggedUser
import com.guizmaii.zio.htmx.services.{SessionManager, UsersService}
import com.guizmaii.zio.htmx.ui.Toast
import zio.ZIO
import zio.http.*
import zio.http.extensions.RichResponseType

object Router {

  val publicRoutes: HttpApp[UsersService & SessionManager] =
    Routes(
      Method.GET / trailing            ->
        Handler.fromFunctionZIO { (req: Request) =>
          for {
            filter          <- ZIO.succeed(req.url.queryParams.get("search").fold(ifEmpty = "")(_.trim))
            users           <- ZIO.serviceWith[UsersService](_.find(filter))
            maybeLoggedUser <- ZIO.serviceWithZIO[SessionManager](_.loggedUser(req))
          } yield
            if (req.hasHeader("HX-Request")) Response.twirl(partials.html.users_table_body(users))
            else {
              // TODO Jules
              Response.twirl(views.html.index(???)(maybeLoggedUser)(searchValue = Some(filter), users = Some(users)))
            }
        },
      Method.GET / "blog"              -> Handler.response(Response.twirl(views.html.blog())),
      Method.GET / "names"             -> Handler.response(Response.twirl(partials.html.names(List("Pierre", "Paul", "Jacques")))),
      Method.GET / "ping"              -> Handler.response(Response.ok),
      Method.GET / "hello"             -> Handler.response(Response.text("Hello World")),
      Method.GET / "toast"             -> Handler.response(Response.twirl(views.html.toast())),
      Method.GET / "toast" / "primary" ->
        Handler.response(Response.twirl(views.html.toast(Toast.Primary("This is a Primary toast")))),
      Method.GET / "toast" / "success" ->
        Handler.response(Response.twirl(views.html.toast(Toast.Success("This is a Success toast")))),
      Method.GET / "toast" / "neutral" ->
        Handler.response(Response.twirl(views.html.toast(Toast.Neutral("This is a Neutral toast")))),
      Method.GET / "toast" / "warning" ->
        Handler.response(Response.twirl(views.html.toast(Toast.Warning("This is a Warning toast")))),
      Method.GET / "toast" / "error"   ->
        Handler.response(Response.twirl(views.html.toast(Toast.Error("This is an Error toast")))),
    ).toHttpApp

  def authenticatedRoutes(authMiddleware: HandlerAspect[Any, LoggedUser]): HttpApp[Any] =
    Routes(
      Method.GET / "profile" -> authMiddleware ->
        Handler.fromFunction[(LoggedUser, Request)] { case (user, _) => Response.twirl(views.html.profile(user)) }
    ).toHttpApp
}
