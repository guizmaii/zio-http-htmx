package zio.http

import play.twirl.api.HtmlFormat
import zio.http.Response.BasicResponse

object extensions {

  private val contentTypeHtml: Headers = Headers(Header.ContentType(MediaType.text.html).untyped)

  implicit final class RichResponseType(private val dummy: Response.type) extends AnyVal {
    def twirl(html: HtmlFormat.Appendable, status: Status = Status.Ok): Response =
      new BasicResponse(
        Body.fromCharSequence(html.toString),
        contentTypeHtml,
        status,
      )
  }

}
