package com.guizmaii.zio.htmx.services

import com.guizmaii.zio.htmx.types.NonEmptyString
import com.guizmaii.zio.htmx.utils.Config.*
import io.logto.sdk.core.Core
import io.logto.sdk.core.`type`.{CodeTokenResponse, OidcConfigResponse, RefreshTokenTokenResponse}
import io.logto.sdk.core.exception.ResponseException
import io.logto.sdk.core.util.{CallbackUriUtils, GenerateUtils, ScopeUtils}
import zio.Config.Secret
import zio.config.{ConfigDescriptor, ReadError, ZConfig}
import zio.http.URL
import zio.json.{DecoderOps, DeriveJsonCodec, EncoderOps, JsonCodec}
import zio.{Cause, Task, URLayer, ZIO, ZLayer}

import java.nio.charset.StandardCharsets
import java.util
import java.util.Base64
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

final case class LogToConfig(
  endpoint: NonEmptyString,
  appId: NonEmptyString,
  appSecret: Secret,
  redirectUri: NonEmptyString,
)
object LogToConfig {
  private val config: ConfigDescriptor[LogToConfig] =
    (
      nonEmptyString("LOGTO_ENDPOINT")
        <*> nonEmptyString("LOGTO_APP_ID")
        <*> secret("LOGTO_APP_SECRET") // TODO Jules: Why is this never used? 🤔
        <*> nonEmptyString("LOGTO_REDIRECT_URI")
    ).to[LogToConfig]

  val fromSystemEnv: ZLayer[Any, ReadError[String], LogToConfig] = ZConfig.fromSystemEnv(config)
}

final case class SignInSession(redirectUri: String, state: String, codeVerifier: String)
object SignInSession {
  implicit private val codec: JsonCodec[SignInSession] = DeriveJsonCodec.gen[SignInSession]

  def encode(signInSession: SignInSession): String =
    Base64.getUrlEncoder.encodeToString(signInSession.toJson.getBytes(StandardCharsets.UTF_8))

  def decode(s: String): Either[String, SignInSession] =
    try new String(Base64.getUrlDecoder.decode(s), StandardCharsets.UTF_8).fromJson[SignInSession]
    catch {
      case NonFatal(_) => Left("Invalid cookie encoding")
    }
}

/**
 * Based on https://docs.logto.io/docs/recipes/integrate-logto/traditional/
 */
trait LogToService {
  def getSignInUrl: Task[(SignInSession, URL)]
  def handleSignIn(signInSession: SignInSession, callbackUri: String): Task[CodeTokenResponse]
  def refreshTokens(refreshToken: String): Task[RefreshTokenTokenResponse]
}

object LogToService {
  val live: URLayer[LogToConfig, LogToService] =
    ZLayer.fromZIO {
      for {
        config <- ZIO.service[LogToConfig]
      } yield new LogToServiceLive(config)
    }
}

final class LogToServiceLive(config: LogToConfig) extends LogToService {

  // See:
  //   - https://github.com/logto-io/kotlin/blob/bced03d3ec169b4b774267154473175b21bec649/kotlin-sdk/kotlin/src/main/kotlin/io/logto/sdk/core/util/ScopeUtils.kt#L6
  //   - https://github.com/logto-io/kotlin/blob/bced03d3ec169b4b774267154473175b21bec649/kotlin-sdk/kotlin/src/test/kotlin/io/logto/sdk/core/util/ScopeUtilsTest.kt#L20
  private val scopes: util.List[String] = ScopeUtils.INSTANCE.withDefaultScopes(List("email").asJava)

  // TODO Jules: use newtypes
  override def getSignInUrl: Task[(SignInSession, URL)] =
    (
      for {
        oidcConfig   <- fetchOidcConfig
        codeVerifier  = GenerateUtils.INSTANCE.generateCodeVerifier()
        codeChallenge = GenerateUtils.INSTANCE.generateCodeChallenge(codeVerifier)
        state         = GenerateUtils.INSTANCE.generateState()
        signInUri    <- ZIO.attempt {
                          // Might throw
                          Core.INSTANCE.generateSignInUri(
                            oidcConfig.getAuthorizationEndpoint,
                            config.appId,
                            config.redirectUri,
                            codeChallenge,
                            state,
                            scopes,
                            null, // resources
                            null, // prompt
                          )
                        }
        _            <- ZIO.logDebug(s"Generated sign-in URI: $signInUri")
        parsed       <- ZIO.fromEither(URL.decode(signInUri))
      } yield (SignInSession(redirectUri = config.redirectUri, state = state, codeVerifier = codeVerifier), parsed)
    ).logError("Error while generating the sign-in URL")

  override def handleSignIn(signInSession: SignInSession, callbackUri: String): Task[CodeTokenResponse] =
    (
      for {
        code              <- ZIO.attempt {
                               // See https://github.com/logto-io/kotlin/issues/215
                               CallbackUriUtils.INSTANCE.verifyAndParseCodeFromCallbackUri(callbackUri, signInSession.redirectUri, signInSession.state)
                             }
        _                 <- ZIO.logDebug(s"Code parsed from callback URI: $code")
        oidcConfig        <- fetchOidcConfig
        codeTokenResponse <- ZIO.asyncZIO[Any, Throwable, CodeTokenResponse] { cb =>
                               ZIO.attempt {
                                 Core.INSTANCE.fetchTokenByAuthorizationCode(
                                   oidcConfig.getTokenEndpoint,
                                   config.appId,
                                   signInSession.redirectUri,
                                   signInSession.codeVerifier,
                                   code,
                                   null, // resource
                                   (throwable: Throwable, t: CodeTokenResponse) =>
                                     if (throwable ne null) {
                                       println("========================================= TATA =================================")
                                       println(throwable)
                                       println(throwable.asInstanceOf[ResponseException].getResponseMessage)
                                       println(throwable.asInstanceOf[ResponseException].getResponseContent.)
                                       println("========================================= ==== =================================")
                                       cb(ZIO.fail(throwable))
                                     } else cb(ZIO.succeed(t)),
                                 )
                               }
                             }
        _                 <- ZIO.logDebug(s"Code token response: $codeTokenResponse")
      } yield codeTokenResponse
    ).logError("Error while handling the sign-in")

  override def refreshTokens(refreshToken: String): Task[RefreshTokenTokenResponse] =
    (
      for {
        oidcConfig        <- fetchOidcConfig
        codeTokenResponse <- ZIO.asyncZIO[Any, Throwable, RefreshTokenTokenResponse] { cb =>
                               ZIO.attempt {

                                 Core.INSTANCE.fetchTokenByRefreshToken(
                                   oidcConfig.getTokenEndpoint,
                                   config.appId,
                                   refreshToken,
                                   null, // resource
                                   scopes,
                                   (throwable: Throwable, t: RefreshTokenTokenResponse) =>
                                     if (throwable ne null) cb(ZIO.fail(throwable)) else cb(ZIO.succeed(t)),
                                 )
                               }
                             }
      } yield codeTokenResponse
    ).logError("Error while refreshing the tokens")

  private def fetchOidcConfig: Task[OidcConfigResponse] =
    ZIO
      .asyncZIO[Any, Throwable, OidcConfigResponse] { cb =>
        ZIO.attempt {
          Core.INSTANCE.fetchOidcConfig(
            // Comes from https://docs.logto.io/docs/references/sdk-convention/core-sdk-convention/#core-functions
            s"${config.endpoint}/oidc/.well-known/openid-configuration",
            (throwable: Throwable, t: OidcConfigResponse) => if (throwable ne null) cb(ZIO.fail(throwable)) else cb(ZIO.succeed(t)),
          )
        }
      }
      .tapBoth(
        e => ZIO.logErrorCause("Error while fetching OIDC config", Cause.fail(e)),
        v => ZIO.logDebug(s"OIDC config fetched: $v"),
      )
}
