package com.guizmaii.zio.htmx

import zio.durationInt
import zio.http.*
import zio.http.HttpAppMiddleware.*
import zio.http.extensions.RichResponseType
import zio.http.internal.middlewares.Cors.CorsConfig

object Router {

  private val corsConfig: CorsConfig = CorsConfig(allowedOrigin = _ => Some(Header.AccessControlAllowOrigin.All))

  val routes: App[Any] =
    Http.collect[Request] {
      case Method.GET -> Root           => Response.twirl(views.html.index())
      case Method.GET -> Root / "blog"  => Response.twirl(views.html.blog())
      case Method.GET -> Root / "names" => Response.twirl(partials.html.names(List("Pierre", "Paul", "Jacques")))
      case Method.GET -> Root / "ping"  => Response.ok
      case Method.GET -> Root / "hello" => Response.text("Hello World")
    } @@ cors(corsConfig) @@ debug @@ timeout(5.seconds)

}
