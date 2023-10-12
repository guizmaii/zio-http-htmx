import BuildHelper.*
import Libraries.*

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion               := "2.13.12"
ThisBuild / version                    := "0.1.0-SNAPSHOT"
ThisBuild / organization               := "com.guizmaii.zio.htmx"
ThisBuild / scalafmtCheck              := true
ThisBuild / scalafmtSbtCheck           := true
ThisBuild / scalafmtOnCompile          := !insideCI.value
ThisBuild / semanticdbEnabled          := true
ThisBuild / semanticdbOptions += "-P:semanticdb:synthetics:on"
ThisBuild / semanticdbVersion          := scalafixSemanticdb.revision // use Scalafix compatible version
ThisBuild / scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value)
ThisBuild / scalafixDependencies ++= List(
  "com.github.vovapolu"                      %% "scaluzzi" % "0.1.23",
  "io.github.ghostbuster91.scalafix-unified" %% "unified"  % "0.0.9",
)

ThisBuild / resolvers ++= Resolver.sonatypeOssRepos("staging")

// ### Aliases ###

addCommandAlias("tc", "Test/compile")
addCommandAlias("ctc", "clean; Test/compile")
addCommandAlias("rctc", "reload; clean; Test/compile")
addCommandAlias("start", "~root/reStart")
addCommandAlias("stop", "reStop")
addCommandAlias("restart", "stop;start")
addCommandAlias("rst", "restart")

// ### App Modules ###

lazy val root =
  (project in file("."))
    .enablePlugins(SbtTwirl)
    .settings(stdSettings*)
    .settings(Revolver.enableDebugging())
    .settings(reLogTag := "zio-http-htmx")
    .settings(
      name := "zio-http-htmx",
      libraryDependencies ++= Seq(zioHttp) ++ loggingRuntime ++ zioConfig,
      testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    )

