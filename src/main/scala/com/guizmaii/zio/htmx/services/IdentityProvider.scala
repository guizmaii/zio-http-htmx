package com.guizmaii.zio.htmx.services

import com.guizmaii.zio.htmx.types.NonEmptyString
import com.guizmaii.zio.htmx.utils.Config.*
import zio.Config.Secret
import zio.config.{ConfigDescriptor, ReadError, ZConfig}
import zio.http.*
import zio.json.{DecoderOps, DeriveJsonDecoder, JsonDecoder}
import zio.prelude.BicovariantOps
import zio.{Task, URLayer, ZEnvironment, ZIO, ZLayer}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.security.SecureRandom
import java.util.Base64

final case class CodeTokenResponse(
  access_token: String,
  refresh_token: String,
  id_token: String,
  scope: String,
  expires_in: Long,
) {
  @inline def accessToken: String  = access_token
  @inline def refreshToken: String = refresh_token
  @inline def idToken: String      = id_token
  @inline def expiresIn: Long      = expires_in
}
object CodeTokenResponse {
  implicit val decoder: JsonDecoder[CodeTokenResponse] = DeriveJsonDecoder.gen[CodeTokenResponse]
}

final case class KindeConfig(
  domain: NonEmptyString,
  clientId: NonEmptyString,
  clientSecret: Secret,
  callbackUrl: NonEmptyString,
  logoutRedirectUrl: NonEmptyString,
)
object KindeConfig       {
  private val config: ConfigDescriptor[KindeConfig] =
    (
      nonEmptyString("KINDE_DOMAIN")
        <*> nonEmptyString("KINDE_CLIENT_ID")
        <*> secret("KINDE_CLIENT_SECRET")
        <*> nonEmptyString("KINDE_CALLBACK_URL")
        <*> nonEmptyString("KINDE_LOGOUT_REDIRECT_URL")
    ).to[KindeConfig]

  val fromSystemEnv: ZLayer[Any, ReadError[String], KindeConfig] = ZConfig.fromSystemEnv(config)
}

trait IdentityProvider {
  def getSignInUrl: (URL, String)
  def handleSignIn(code: String): Task[CodeTokenResponse]
  def refreshTokens(refreshToken: String): Task[CodeTokenResponse]
  def logoutUrl: URL
}

object IdentityProvider {

  /**
   * https://kinde.com/
   */
  val kinde: URLayer[KindeConfig & Client, IdentityProvider] =
    ZLayer.fromZIO {
      for {
        config      <- ZIO.service[KindeConfig]
        client      <- ZIO.environment[Client]
        secureRandom = SecureRandom.getInstance("NativePRNGNonBlocking")
      } yield new Kinde(config, client, secureRandom)
    }
}

/**
 * Implementation based on:
 *  - information from: https://kinde.com/docs/developer-tools/using-kinde-without-an-sdk/
 *  - TS SDK: https://github.com/kinde-oss/kinde-typescript-sdk
 */
final class Kinde(config: KindeConfig, client: ZEnvironment[Client], secureRandom: SecureRandom) extends IdentityProvider {

  /**
   * `%20` is the `+` character url encoded
   */
  private val scopes: String     = "openid%20offline%20profile%20email"
  private val authUrl: String    = s"${config.domain}/oauth2/auth"
  private val tokenUrl: URL      = URL.decode(s"${config.domain}/oauth2/token").fold(throw _, identity) // throw side should never happen
  private val clientSecret       = config.clientSecret.value.toArray.mkString
  private val encodedRedirectUri = URLEncoder.encode(config.callbackUrl, StandardCharsets.UTF_8)

  override val getSignInUrl: (URL, String) = {
    // The state NEEDS to be generated out of a secure random generator
    // See: https://medium.com/keycloak/the-importance-of-the-state-parameter-in-oauth-5419c94bef4c
    //
    // Quoting the article:
    // > The state parameter should be seeded with a secure random (this avoids CSRF attacks)
    //
    // Also, note that the official Kinde Ruby SDK uses a 16 characters long state.
    // See:
    //   - https://github.com/kinde-oss/kinde-ruby-sdk/blob/main/lib/kinde_sdk.rb#L33
    //   - https://apidock.com/ruby/SecureRandom/hex/class
    // Here, with our 64 bytes long state, we have generate a 88 characters long state.
    // See: https://scastie.scala-lang.org/XpObe0sMQzqvGBZQqdOB1Q
    // That's probably plenty enough seeing the state size used in the official SDK.
    //
    val array = new Array[Byte](64)
    secureRandom.nextBytes(array)
    val state = Base64.getUrlEncoder.encodeToString(array)

    URL
      .decode(
        s"$authUrl?response_type=code&client_id=${config.clientId}&redirect_uri=$encodedRedirectUri&scope=$scopes&state=$state"
      )
      .fold(throw _, _ -> state) // The throw part should never happen
  }

  /**
   * Raw response looks like this:
   *
   * {{{
   *  {
   *    "access_token": "abcdefgiJSUzI1NiIsImtpZCI6IjIwOjBhOjE2OmZmOmFkOmYxOjQ0OjgyOjQ3OjJlOmUwOjNkOmEwOjExOjMxOjM3IiwidHlwIjoiSldUIn0.eyJhdWQiOltdLCJhenAiOiI0ZTg0MDU2Zjc4M2Q0ZTFmYmIyYzkyNTA3YzU4YWRkMiIsImV4cCI6MTY5NzE5MDYzMiwiaWF0IjoxNjk3MTA0MjMyLCJpc3MiOiJodHRwczovL3pla2xpbi1zdGFnaW5nLmV1LmtpbmRlLmNvbSIsImp0aSI6ImRkNWU4YjY0LTBlNWQtNDE4Mi1iMGNjLTJiYjlhMTE5NDM1OSIsIm9yZ19jb2RlIjoib3JnXzU3NmEzODNhYTQ2IiwicGVybWlzc2lvbnMiOltdLCJzY3AiOlsib3BlbmlkIiwib2ZmbGluZSIsInByb2ZpbGUiLCJlbWFpbCJdLCJzdWIiOiJrcF8wYzYxNDk2ODhjOGQ0NGVkOGE4ZGJlMGFlZDZjNGJjYSJ9.NGQ_KmIkVWPpe_oj4Lxj7ueai1bycrmbsZa-qjpexArGaqfibK5L9JSFXdg97q9JZfkJpdqgZQOHKP6UqpdBClVJzJKhllMLVGrsv6BsjJLGtnbTWrspXdoMn26Twmzu_eUIlwBEVHwmwwIBQyzRiUQlVRlEQP0G4hVfy5vkIZpLeIqhDXWC4SluSVRa8nJ5oiYXRrqYcWdlBbXi8Irc0RXlZljkhxhGQBm31CZPZH5xFnUf9aEA5mIUFlEp7i3nh92-LRqqYSbkynd7SOlfzvYWfC2-vawf2WgqGPAGAzH_ZlbI59z8c2i3mobIEa9Tpe1tiwfEUjxTI_dslfbVpQ",
   *    "expires_in": 86399,
   *    "id_token": "abcdefgJSUzI1NiIsImtpZCI6IjIwOjBhOjE2OmZmOmFkOmYxOjQ0OjgyOjQ3OjJlOmUwOjNkOmEwOjExOjMxOjM3IiwidHlwIjoiSldUIn0.eyJhdF9oYXNoIjoiNzdabWY2bkVCY2Zfa1NDZlB3dkF2dyIsImF1ZCI6WyJodHRwczovL3pla2xpbi1zdGFnaW5nLmV1LmtpbmRlLmNvbSIsIjRlODQwNTZmNzgzZDRlMWZiYjJjOTI1MDdjNThhZGQyIl0sImF1dGhfdGltZSI6MTY5NzEwNDIzMiwiYXpwIjoiNGU4NDA1NmY3ODNkNGUxZmJiMmM5MjUwN2M1OGFkZDIiLCJlbWFpbCI6Imp1bGVzLml2YW5pY0BnbWFpbC5jb20iLCJleHAiOjE2OTcxMDc4MzIsImZhbWlseV9uYW1lIjoiSXZhbmljIiwiZ2l2ZW5fbmFtZSI6Ikp1bGVzIiwiaWF0IjoxNjk3MTA0MjMyLCJpc3MiOiJodHRwczovL3pla2xpbi1zdGFnaW5nLmV1LmtpbmRlLmNvbSIsImp0aSI6IjZmYzhkMmMyLTYyNmYtNGVjYi04YzBhLTBhNDA5OTdmMTQ4NyIsIm5hbWUiOiJKdWxlcyBJdmFuaWMiLCJvcmdfY29kZXMiOlsib3JnXzU3NmEzODNhYTQ2Il0sInN1YiI6ImtwXzBjNjE0OTY4OGM4ZDQ0ZWQ4YThkYmUwYWVkNmM0YmNhIiwidXBkYXRlZF9hdCI6MS42OTcwMzIxNzJlKzA5fQ.JT-ZFYDrZ9T-qXGYkyeVPDJOIeMbfNzKYcmWnf9Jlk-_Nf_V_AK_8hoe5PbcSDjJGFR2YGeB2VCrV5PaFKDlZ9Id0U5BJSSJRCHtGuvIlmx-ISQORL8GFs-kEC9i1z1jpuXkVn1moVttjUbbeIgWmv7vwZtccfhFXVocpQZPUaWdP7vU22soOKGo6AisE7EUGQD_yi6xcbW2eMU1KdjD4tmZAHH1xnCgJLyPoP3fheRMivpWCl5zJseox63TmEUFSV6qWoZjQ-vaO9LTiMHiwxQPpxd1Y3-fdk_RvJefLYHQ8xofcKYQIG8Xb08a2T0qEOWAsGvMwJyQ2GXyFiwp_A",
   *    "refresh_token": "abcdefgFDyzGh7fvKwMiofL8WMlKOPw6wqQ0rRcc4.Jib6ZorEt4DmqxMMpoo0PrPYKUgKeNhKO3w5mSB2pXU",
   *    "scope": "openid offline profile email",
   *    "token_type": "bearer"
   *  }
   * }}}
   */
  override def handleSignIn(code: String): Task[CodeTokenResponse] =
    (
      for {
        response <- Client.request(
                      Request(
                        method = Method.POST,
                        url = tokenUrl,
                        headers = Headers(
                          Header.ContentType(MediaType.application.`x-www-form-urlencoded`, charset = Some(StandardCharsets.UTF_8)),
                          Header.Custom("Jules-SDK", "Scala/0.0.0"),
                        ),
                        body = Body.fromURLEncodedForm(
                          Form.fromStrings(
                            "grant_type"    -> "authorization_code",
                            "client_id"     -> config.clientId,
                            "client_secret" -> clientSecret,
                            "redirect_uri"  -> config.callbackUrl,
                            "code"          -> code,
                          )
                        ),
                        version = Version.Http_1_1,
                        remoteAddress = None,
                      )
                    )
        raw      <- if (response.status.isSuccess) response.body.asString
                    else {
                      response.body.asString.flatMap { body =>
                        val error = s"Error while getting the tokens: ${response.status.code} - ${response.headers.toString()} - $body"
                        ZIO.logDebug(error) *> ZIO.fail(new RuntimeException(error))
                      }
                    }
        _        <- ZIO.logDebug(s"Raw `authorization_code` response from Kinde - $raw")
        parsed   <- ZIO.fromEither(raw.fromJson[CodeTokenResponse].leftMap(new RuntimeException(_)))
      } yield parsed
    ).logError("Error while handling the sign-in")
      .provideEnvironment(client)

  override def refreshTokens(refreshToken: String): Task[CodeTokenResponse] =
    (
      for {
        response <- Client.request(
                      Request(
                        method = Method.POST,
                        url = tokenUrl,
                        headers = Headers(
                          Header.ContentType(MediaType.application.`x-www-form-urlencoded`, charset = Some(StandardCharsets.UTF_8)),
                          Header.Custom("Jules-SDK", "Scala/0.0.0"),
                        ),
                        body = Body.fromURLEncodedForm(
                          Form.fromStrings(
                            "grant_type"    -> "refresh_token",
                            "client_id"     -> config.clientId,
                            "client_secret" -> clientSecret,
                            "refresh_token" -> refreshToken,
                          )
                        ),
                        version = Version.Http_1_1,
                        remoteAddress = None,
                      )
                    )
        raw      <- if (response.status.isSuccess) response.body.asString
                    else {
                      response.body.asString.flatMap { body =>
                        val error = s"Error while refreshing a token: ${response.status.code} - ${response.headers.toString()} - $body"
                        ZIO.logDebug(error) *> ZIO.fail(new RuntimeException(error))
                      }
                    }
        _        <- ZIO.logDebug(s"Raw `refresh_token` response from Kinde - $raw")
        parsed   <- ZIO.fromEither(raw.fromJson[CodeTokenResponse].leftMap(new RuntimeException(_)))
      } yield parsed
    ).logError("Error while refreshing a token")
      .provideEnvironment(client)

  override val logoutUrl: URL =
    URL.decode(s"${config.domain}/logout?redirect=${config.logoutRedirectUrl}").fold(throw _, identity) // throw side should never happen
}
