package com.guizmaii.zio.htmx.utils

final case class ShouldNeverHappen(message: String, cause: Throwable = null)
    extends RuntimeException(s"SHOULD NEVER HAPPEN: $message", cause)
