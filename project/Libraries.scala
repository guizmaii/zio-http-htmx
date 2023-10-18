import sbt.*

object Libraries {

  val zioVersion       = "2.0.18"
  val slf4jVersion     = "2.0.9"
  val zioConfigVersion = "3.0.7"
  val zioHttpVersion   = "3.0.0-RC2+115-3a6525ce-SNAPSHOT"
  val zioJsonVersion   = "0.6.2"

  val zio        = "dev.zio" %% "zio"                % zioVersion
  val prelude    = "dev.zio" %% "zio-prelude"        % "1.0.0-RC21"
  val zioHttp    = "dev.zio" %% "zio-http"           % zioHttpVersion
  val zioLogging = "dev.zio" %% "zio-logging-slf4j2" % "2.1.14"
  val zioJson    = "dev.zio" %% "zio-json"           % zioJsonVersion

  val tests = Seq(
    "dev.zio" %% "zio-test"          % zioVersion,
    "dev.zio" %% "zio-test-sbt"      % zioVersion,
    "dev.zio" %% "zio-test-magnolia" % zioVersion,
    "dev.zio" %% "zio-mock"          % "1.0.0-RC11",
  )

  val loggingRuntime = Seq(
    "ch.qos.logback"       % "logback-classic"          % "1.4.11",
    "net.logstash.logback" % "logstash-logback-encoder" % "7.4",
    "org.slf4j"            % "jul-to-slf4j"             % slf4jVersion,
    "org.slf4j"            % "log4j-over-slf4j"         % slf4jVersion,
    "org.slf4j"            % "jcl-over-slf4j"           % slf4jVersion,
    "org.slf4j"            % "slf4j-api"                % slf4jVersion,
  )

  val zioConfig = Seq(
    "dev.zio" %% "zio-config"          % zioConfigVersion,
    "dev.zio" %% "zio-config-magnolia" % zioConfigVersion,
    "dev.zio" %% "zio-config-typesafe" % zioConfigVersion,
  )

}
