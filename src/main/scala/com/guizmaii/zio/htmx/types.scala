package com.guizmaii.zio.htmx

import zio.prelude.{AssertionError, Subtype}

object types {

  val nonEmptyStringAssertion: String => Either[AssertionError, Unit] =
    (s: String) =>
      if (s.isBlank) Left(AssertionError.failure("NonEmptyString cannot be empty"))
      else Right(())

  type NonEmptyString = NonEmptyString.Type
  object NonEmptyString extends Subtype[String] {
    // noinspection TypeAnnotation
    override def assertion = assertCustom(nonEmptyStringAssertion)
  }

  type CookieSignKey = CookieSignKey.Type
  object CookieSignKey extends Subtype[String] {
    // noinspection TypeAnnotation
    override def assertion = assertCustom(nonEmptyStringAssertion)
  }

}
