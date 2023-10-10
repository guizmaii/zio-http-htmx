package com.guizmaii.zio.htmx

import com.guizmaii.zio.htmx.services.UsersService
import zio.ZIO
import zio.http.*
import zio.http.extensions.RichResponseType

import java.util.UUID

object Router {

  val routes: App[UsersService] =
    Http.collectZIO[Request] {
      case req @ Method.GET -> Root            =>
        req.url.queryParams.get("search").flatMap(_.headOption).map(_.trim) match {
          case None         => ZIO.succeed(Response.twirl(views.html.index()))
          case Some(filter) =>
            for {
              users <- ZIO.serviceWith[UsersService](_.find(filter))
            } yield
              if (req.hasHeader("HX-Request")) Response.twirl(partials.html.users_table_body(users))
              else Response.twirl(views.html.index(searchValue = Some(filter), users = Some(users)))
        }
      case Method.GET -> Root / "blog"         => ZIO.succeed(Response.twirl(views.html.blog()))
      case req @ Method.GET -> Root / "cookie" =>
        ZIO.succeed {
          // Adapted from https://zio.dev/zio-http/dsl/cookies/#getting-cookie-from-request
          val sessionId = req.header(Header.Cookie).map(_.value.head.content)

          Response.twirl(views.html.cookie(sessionId))
        }

      case req @ Method.POST -> Root / "login" =>
        ZIO.succeed {
          val cookie = Cookie.Response(name = "zio-http-htmx", content = UUID.randomUUID().toString)
          val origin = req.headers.get("Hx-Current-Url").flatMap(URL.decode(_).toOption).getOrElse(URL.root)
          Response.redirect(origin).addCookie(cookie)
        }

      case req @ Method.POST -> Root / "logout" =>
        ZIO.succeed {
          val cookie = Cookie.Response(name = "zio-http-htmx", content = "")
          val origin = req.headers.get("Hx-Current-Url").flatMap(URL.decode(_).toOption).getOrElse(URL.root)

          Response.redirect(origin).addCookie(cookie)
        }
      case Method.GET -> Root / "names"         => ZIO.succeed(Response.twirl(partials.html.names(List("Pierre", "Paul", "Jacques"))))
      case Method.GET -> Root / "ping"          => ZIO.succeed(Response.ok)
      case Method.GET -> Root / "hello"         => ZIO.succeed(Response.text("Hello World"))
    }

}
