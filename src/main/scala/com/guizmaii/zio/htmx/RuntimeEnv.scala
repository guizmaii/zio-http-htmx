package com.guizmaii.zio.htmx

sealed trait RuntimeEnv {
  def isProdLike: Boolean =
    this match {
      case RuntimeEnv.Dev        => false
      case RuntimeEnv.Staging    => true
      case RuntimeEnv.Production => true
    }
}
object RuntimeEnv       {
  case object Dev        extends RuntimeEnv
  case object Staging    extends RuntimeEnv
  case object Production extends RuntimeEnv

  def fromString(env: String): RuntimeEnv =
    env.toLowerCase match {
      case "dev"        => Dev
      case "staging"    => Staging
      case "production" => Production
      case _            => throw new IllegalArgumentException(s"Unknown environment: $env") // Lazy 😄
    }
}
