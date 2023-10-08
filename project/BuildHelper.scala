import _root_.org.typelevel.sbt.tpolecat.TpolecatPlugin.autoImport.*
import Libraries.*
import org.typelevel.scalacoptions.ScalacOptions
import sbt.*
import sbt.Keys.*

object BuildHelper {

  private val javaTarget = "21"

  def env(v: String): Option[String] = sys.env.get(v)
  def unsafeEnv(v: String): String   = sys.env(v)

  lazy val stdSettings =
    noDoc ++ Seq(
      addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.13.2" cross CrossVersion.full),
      addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1"),
      addCompilerPlugin("com.hmemcpy"   %% "zio-clippy"         % "0.0.5"),
      libraryDependencies ++= Seq(zio, prelude, zioLogging) ++ tests.map(_ % Test),
      scalacOptions += "-P:clippy:show-original-error",                         // See https://github.com/hmemcpy/zio-clippy#additional-configuration
      scalacOptions -= "-Xlint:infer-any",                                      // Disable "a type was inferred to be `Any`" check which doesn't work well
      javacOptions ++= Seq("-source", javaTarget, "-target", javaTarget),
      scalacOptions ++= Seq(
        "-Ymacro-annotations",
        "-Xsource:3",
        s"-release:$javaTarget",
        "-Wconf:src=twirl/.*:is", // https://github.com/playframework/twirl/issues/105#issuecomment-1012413623
      ),
      scalacOptions --= (if (insideCI.value) Nil else Seq("-Xfatal-warnings")), // enforced by the pre-push hook too
      // format: off
      tpolecatScalacOptions ++= Set(
        ScalacOptions.privateBackendParallelism(), // See https://github.com/typelevel/sbt-tpolecat/blob/main/plugin/src/main/scala/io/github/davidgregory084/ScalacOptions.scala#L409-L424
      ),
      // format: on
    )

  lazy val noDoc = Seq(
    (Compile / doc / sources)                := Seq.empty,
    (Compile / packageDoc / publishArtifact) := false,
  )

  /**
   * Copied from Cats
   */
  lazy val noPublishSettings = Seq(
    publish         := {},
    publishLocal    := {},
    publishM2       := {},
    publishArtifact := false,
  )

}
