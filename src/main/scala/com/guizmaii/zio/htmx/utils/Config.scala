package com.guizmaii.zio.htmx.utils

import com.guizmaii.zio.htmx.types.NonEmptyString
import zio.Config.Secret
import zio.config.*
import zio.config.ConfigDescriptor.*
import zio.prelude.Newtype

object Config {

  def trimmedString(path: String): ConfigDescriptor[String]          = string(path).map(_.trim)
  def nonEmptyString(path: String): ConfigDescriptor[NonEmptyString] = trimmedString(path).toSubType(NonEmptyString)
  def secret(path: String): ConfigDescriptor[Secret]                 = nonEmptyString(path).map(Secret(_))

  implicit final class ToNewTypeWrapper[T](configDescriptor: ConfigDescriptor[T]) {
    def toNewType: AppliedToNewTypeWrapper =
      new AppliedToNewTypeWrapper(configDescriptor)

    def toSubType: AppliedToNewTypeWrapper =
      toNewType

    final class AppliedToNewTypeWrapper(configDescriptor: ConfigDescriptor[T]) {
      def apply[N <: Newtype[T]](newType: N): ConfigDescriptor[newType.Type] =
        configDescriptor.transformOrFailLeft[newType.Type](newType.make(_).toEitherWith(_.mkString("; ")))(newType.unwrap)
    }
  }
}
