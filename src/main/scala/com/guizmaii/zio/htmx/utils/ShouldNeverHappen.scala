package com.guizmaii.zio.htmx.utils

final case class ShouldNeverHappen(message: String, cause: Throwable = null) extends RuntimeException(message, cause)
