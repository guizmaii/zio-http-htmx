package com.guizmaii.zio.htmx

import com.guizmaii.zio.htmx.services.IdToken
import zio.Scope
import zio.json.{DecoderOps, DeriveJsonDecoder, JsonDecoder}
import zio.test.*
import zio.test.Assertion.*

object IdTokenSpec extends ZIOSpecDefault {

  private val decodeSpec =
    suite(".decoder")(
      test("truth")(assertTrue(true)),
      test("correctly decodes") {
        final case class Toto(id_token: IdToken)
        object Toto {
          implicit val decoder: JsonDecoder[Toto] = DeriveJsonDecoder.gen[Toto]
        }

        val encoded: String =
          """{"id_token": "eyJhbGciOiJSUzI1NiIsImtpZCI6IjIwOjBhOjE2OmZmOmFkOmYxOjQ0OjgyOjQ3OjJlOmUwOjNkOmEwOjExOjMxOjM3IiwidHlwIjoiSldUIn0.eyJhdF9oYXNoIjoic1dvREdJQ3UzWWNZWDlfdENJZEUwUSIsImF1ZCI6WyJodHRwczovL3pla2xpbi1zdGFnaW5nLmV1LmtpbmRlLmNvbSIsIjRlODQwNTZmNzgzZDRlMWZiYjJjOTI1MDdjNThhZGQyIl0sImF1dGhfdGltZSI6MTY5NzE4NDE1NiwiYXpwIjoiNGU4NDA1NmY3ODNkNGUxZmJiMmM5MjUwN2M1OGFkZDIiLCJlbWFpbCI6Imp1bGVzLml2YW5pY0BnbWFpbC5jb20iLCJleHAiOjE2OTcxODc3NTYsImZhbWlseV9uYW1lIjoiSXZhbmljIiwiZ2l2ZW5fbmFtZSI6Ikp1bGVzIiwiaWF0IjoxNjk3MTg0MTU2LCJpc3MiOiJodHRwczovL3pla2xpbi1zdGFnaW5nLmV1LmtpbmRlLmNvbSIsImp0aSI6ImUzNDFhYTBkLTQwN2EtNDY4OS05ODBhLTAzOWQxNGQwYjNmYiIsIm5hbWUiOiJKdWxlcyBJdmFuaWMiLCJvcmdfY29kZXMiOlsib3JnXzU3NmEzODNhYTQ2Il0sInBpY3R1cmUiOiJodHRwczovL2F2YXRhcnMuZ2l0aHVidXNlcmNvbnRlbnQuY29tL3UvMTE5MzY3MD92PTQiLCJzdWIiOiJrcF8wYzYxNDk2ODhjOGQ0NGVkOGE4ZGJlMGFlZDZjNGJjYSIsInVwZGF0ZWRfYXQiOjEuNjk3MTI1MTQ1ZSswOX0.dT4lSzzYXNbZ03vElGIeMC8pB2tOdDhK61j9_MX6sVfXGRDH8Z_mdK43VtoUgj6SF6ie-Mrmt8vj667gOaajBbsUNFh0Qkp4TpsqHdHx2VPlbSdOhBDBgldISnP99t5BK-EQiZO2KwtSPcqu_TgQtfOwbsnMeMdNEDa8qEWUic5lND9rs4aZP45cIsfSHkTSGs8NY1fb0bCqommTNDLgnPTm2xg9TnpwUNSx8P4zFi1ZkdVfhlRCO54S_vq2kyHDvlgrDxnGU06zGkIo8J5HUIxwv9RbPvxTiczHvkBSdw8MRRF3O903T9VHlWsQvs5aDsB8m81Av15VmYO1dMpXwA" }"""

        val expected: IdToken =
          IdToken(
            sub = "kp_0c6149688c8d44ed8a8dbe0aed6c4bca",
            given_name = "Jules",
            family_name = "Ivanic",
            name = "Jules Ivanic",
            email = "jules.ivanic@gmail.com",
            picture = Some("https://avatars.githubusercontent.com/u/1193670?v=4"),
          )

        val decoded: Either[String, Toto] = encoded.fromJson[Toto]

        assert(decoded.map(_.id_token))(isRight(equalTo(expected)))
      },
    )

  override def spec: Spec[TestEnvironment & Scope, Any] =
    suite("IdToken")(
      decodeSpec
    )
}
