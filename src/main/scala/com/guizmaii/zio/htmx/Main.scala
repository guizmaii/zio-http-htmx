package com.guizmaii.zio.htmx

import com.guizmaii.zio.htmx.persistence.SessionStorage
import com.guizmaii.zio.htmx.services.{IdentityProvider, KindeConfig, SessionManager, UsersService}
import zio.*
import zio.http.*
import zio.http.HttpAppMiddleware.*
import zio.http.Server.{Config, RequestStreaming}
import zio.http.internal.middlewares.Cors.CorsConfig
import zio.logging.backend.SLF4J

import java.lang.Runtime as JRuntime
import java.net.InetSocketAddress

object Main extends ZIOAppDefault {

  /**
   * See
   *  - `zio-logging` documentation: https://zio.github.io/zio-logging/docs/overview/overview_slf4j
   *  - https://x.com/guizmaii/status/1703395450571694425?s=20
   */
  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
    (zio.Runtime.removeDefaultLoggers >>> SLF4J.slf4j) ++
      Runtime.setExecutor(Executor.makeDefault(autoBlocking = false))

  // TODO: Not prod ready
  private val corsConfig: CorsConfig = CorsConfig(allowedOrigin = _ => Some(Header.AccessControlAllowOrigin.All))

  private val bootSequence: ZIO[Any, Throwable, Unit] =
    for {
      _ <- ZIO.logInfo(s"Runtime.version:             ${JRuntime.version()}".trim)
      _ <- ZIO.logInfo(s"Runtime.availableProcessors: ${JRuntime.getRuntime.availableProcessors()}".trim)
      _ <- ZIO.logInfo(s"Runtime.maxMemory:           ${JRuntime.getRuntime.maxMemory()}".trim)
      _ <- ZIO.logInfo(s"Runtime.totalMemory:         ${JRuntime.getRuntime.totalMemory()}".trim)
      _ <- ZIO.fail(new RuntimeException("Mono-threaded app")).when(JRuntime.getRuntime.availableProcessors() == 1)
    } yield ()

  /**
   * Adapted from [[Server.default]]
   */
  private val server: ZLayer[Any, Throwable, Server] = {
    implicit val trace: Trace = Trace.empty

    ZLayer.fromZIO(
      ZIO.attempt {
        // Adapted from Config.default
        Config(
          sslConfig = None,
          address = new InetSocketAddress(8080),
          acceptContinue = false,
          keepAlive = true,
          requestDecompression = Decompression.No,
          responseCompression = None,
          requestStreaming = RequestStreaming.Disabled(2 * 1024 * 1024), // 2MB
          maxHeaderSize = 8192,
          logWarningOnFatalError = true,
          gracefulShutdownTimeout = 10.seconds,
          webSocketConfig = WebSocketConfig.default,
          idleTimeout = None,
        )
      }
    ) >>> Server.live
  }

  private def app: URIO[SessionManager, App[UsersService & SessionManager]] =
    ZIO.serviceWith[SessionManager] { sessionManager =>
      (
        Router.publicRoutes ++
          AuthRoutes.routes ++
          sessionManager.authMiddleware[Any] {
            Router.authenticatedRoutes
          }
      ) @@ cors(corsConfig) @@ debug @@ timeout(5.seconds)
    }

  override def run: ZIO[Environment & ZIOAppArgs & Scope, Any, Any] =
    (
      bootSequence *> app.flatMap(Server.serve(_))
    ).provide(
      server,
      UsersService.live,
      AppConfig.fromSystemEnv,
      KindeConfig.fromSystemEnv,
      IdentityProvider.kinde,
      Client.default,
      SessionManager.live,
      SessionStorage.inMemory,
      ZLayer.succeed(RuntimeEnv.Dev), // TODO: Not prod ready. Should be loaded from the env for example.
    )
}
