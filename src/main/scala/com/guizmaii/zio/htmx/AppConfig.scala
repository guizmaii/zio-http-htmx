package com.guizmaii.zio.htmx

import com.guizmaii.zio.htmx.types.CookieSignKey
import com.guizmaii.zio.htmx.utils.Config.{ToNewTypeWrapper, trimmedString}
import zio.ZLayer
import zio.config.*

final case class AppConfig(cookieSignKey: CookieSignKey)
object AppConfig {

  private val config: ConfigDescriptor[AppConfig] =
    (
      trimmedString("APP_COOKIE_SIGN_KEY").toSubType(CookieSignKey)
    ).to[AppConfig]

  val fromSystemEnv: ZLayer[Any, ReadError[String], AppConfig] = ZConfig.fromSystemEnv(config)
}
