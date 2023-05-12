/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.site

import io.bullet.borer.internal.Util.*
import io.bullet.borer.BorerSuite

class DomSpec extends BorerSuite {

  test("CBOR") {

    // #cbor
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
    // #cbor
  }

  test("JSON") {

    // #json
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
    // #json
  }

  test("JSON to CBOR") {
    // #json-to-cbor
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
    // #json-to-cbor
  }
}
