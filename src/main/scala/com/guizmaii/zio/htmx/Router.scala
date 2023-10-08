package com.guizmaii.zio.htmx

import com.guizmaii.zio.htmx.services.UsersService
import zio.http.*
import zio.http.HttpAppMiddleware.*
import zio.http.extensions.RichResponseType
import zio.http.internal.middlewares.Cors.CorsConfig
import zio.{ZIO, durationInt}

object Router {

  private val corsConfig: CorsConfig = CorsConfig(allowedOrigin = _ => Some(Header.AccessControlAllowOrigin.All))

  val routes: App[UsersService] =
    Http.collectZIO[Request] {
      case Method.GET -> Root                            => ZIO.succeed(Response.twirl(views.html.index()))
      case Method.GET -> Root / "blog"                   => ZIO.succeed(Response.twirl(views.html.blog()))
      case Method.GET -> Root / "names"                  => ZIO.succeed(Response.twirl(partials.html.names(List("Pierre", "Paul", "Jacques"))))
      case Method.GET -> Root / "users"                  => ZIO.succeed(Response.twirl(partials.html.users()))
      case req @ Method.GET -> Root / "users" / "search" =>
        for {
          users <- ZIO.serviceWith[UsersService] { service =>
                     val filter = req.url.queryParams.get("search").flatMap(_.headOption).map(_.trim)

                     service.find(filter)
                   }
        } yield Response.twirl(partials.html.users_table_body(users))
      case Method.GET -> Root / "ping"                   => ZIO.succeed(Response.ok)
      case Method.GET -> Root / "hello"                  => ZIO.succeed(Response.text("Hello World"))
    } @@ cors(corsConfig) @@ debug @@ timeout(5.seconds)

}
