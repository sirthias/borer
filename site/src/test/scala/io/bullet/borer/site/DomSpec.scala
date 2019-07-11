package io.bullet.borer.site

import io.bullet.borer.internal.Util._
import utest._

object DomSpec extends TestSuite {

  val tests = Tests {

    "CBOR" - {

      //#cbor
      import io.bullet.borer.Cbor
      import io.bullet.borer.Dom._

      val dom = MapElem.Sized(
        "foo" -> ArrayElem.Sized(IntElem(42), StringElem("rocks")),
        "bar" -> DoubleElem(26.8)
      )

      val encoded = Cbor.encode(dom).toByteArray

      encoded ==> hex"A263666F6F82182A65726F636B7363626172FB403ACCCCCCCCCCCD"

      val decoded = Cbor.decode(encoded).to[Element].value

      decoded ==> dom
      //#cbor
    }

    "JSON" - {

      //#json
      import io.bullet.borer.Dom._
      import io.bullet.borer.Json

      val dom = MapElem.Unsized(
        "foo" -> ArrayElem.Unsized(IntElem(42), StringElem("rocks")),
        "bar" -> DoubleElem(26.8)
      )

      val encoded = Json.encode(dom).toByteArray

      new String(encoded, "UTF8") ==> """{"foo":[42,"rocks"],"bar":26.8}"""

      val decoded = Json.decode(encoded).to[Element].value

      decoded ==> dom
      //#json
    }

    "JSON to CBOR" - {
      //#json-to-cbor
      import io.bullet.borer.{Cbor, Json}
      import io.bullet.borer.Dom._

      import scala.util.Try

      def jsonToCbor(json: String): Try[Array[Byte]] =
        Json
          .decode(json getBytes "UTF8")
          .to[Element]
          .valueTry
          .flatMap(dom => Cbor.encode(dom).toByteArrayTry)

      jsonToCbor("""{"foo":[42,"rocks"],"bar":26.8}""").get ==>
      hex"BF63666F6F9F182A65726F636B73FF63626172FB403ACCCCCCCCCCCDFF"
      //#json-to-cbor
    }
  }
}
