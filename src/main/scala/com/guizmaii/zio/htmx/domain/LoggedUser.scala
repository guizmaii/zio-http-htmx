package com.guizmaii.zio.htmx.domain

import com.guizmaii.zio.htmx.services.IdToken
import zio.json.{DeriveJsonCodec, JsonCodec}

final case class LoggedUser(
  identityProviderId: String,
  firstName: String,
  lastName: String,
  name: String,
  email: String,
  picture: Option[String],
)

object LoggedUser {
  implicit val codec: JsonCodec[LoggedUser] = DeriveJsonCodec.gen[LoggedUser]

  def from(idToken: IdToken): LoggedUser =
    LoggedUser(
      identityProviderId = idToken.sub,
      firstName = idToken.given_name,
      lastName = idToken.family_name,
      name = idToken.name,
      email = idToken.email,
      picture = idToken.picture,
    )

}
