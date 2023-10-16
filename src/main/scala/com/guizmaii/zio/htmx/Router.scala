package com.guizmaii.zio.htmx

import com.guizmaii.zio.htmx.services.{SessionManager, UsersService}
import zio.ZIO
import zio.http.*
import zio.http.extensions.RichResponseType

object Router {

  val routes: App[UsersService & SessionManager] =
    Http.collectZIO[Request] {
      case req @ Method.GET -> Root     =>
        ZIO.serviceWithZIO[SessionManager] { sessionManager =>
          for {
            filter          <- ZIO.succeed(req.url.queryParams.get("search").flatMap(_.headOption).fold(ifEmpty = "")(_.trim))
            users           <- ZIO.serviceWith[UsersService](_.find(filter))
            maybeLoggedUser <- ZIO.serviceWithZIO[SessionManager](_.loggedUser(req))
          } yield
            if (req.hasHeader("HX-Request")) Response.twirl(partials.html.users_table_body(users))
            else
              Response.twirl(
                views.html.index(maybeLoggedUser)(searchValue = Some(filter), users = Some(users))
              )
        }
      case Method.GET -> Root / "blog"  => ZIO.succeed(Response.twirl(views.html.blog()))
      case Method.GET -> Root / "names" => ZIO.succeed(Response.twirl(partials.html.names(List("Pierre", "Paul", "Jacques"))))
      case Method.GET -> Root / "ping"  => ZIO.succeed(Response.ok)
      case Method.GET -> Root / "hello" => ZIO.succeed(Response.text("Hello World"))
    }

}
