/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import utest._

object LoggingSpec extends TestSuite {
  import Dom._

  val alphabet = ('A' to 'Z').mkString

  val tests = Tests {

    "simple values" - roundTripLogEquals {
      ArrayElem.Sized(
        NullElem,
        UndefinedElem,
        BoolElem(true),
        IntElem(42),
        LongElem(Int.MaxValue + 1L),
        OverLongElem(negative = false, -1),
        OverLongElem(negative = true, -1),
        Float16Elem(1.0f),
        FloatElem(100000.0f),
        DoubleElem(1.6),
        SimpleValueElem(SimpleValue(18)))
    } {
      """1: [
        |     1/11: null
        |     2/11: undefined
        |     3/11: true
        |     4/11: 42
        |     5/11: 2147483648L
        |     6/11: 0xffffffffffffffff
        |     7/11: -0xffffffffffffffff
        |     8/11: 1.0f16
        |     9/11: 100000.0f
        |    10/11: 1.6
        |    11/11: SimpleValue(18)
        |1: ]
        |2: END
        |""".stripMargin
    }

    "byte arrays" - roundTripLogEquals {
      ArrayElem.Sized(
        ByteArrayElem(alphabet.take(8).getBytes),
        ByteArrayElem(alphabet.getBytes),
        BytesStreamElem(alphabet.grouped(4).map(s ⇒ ByteArrayElem(s.getBytes)).toVector))
    } {
      """1: [
        |    1/3: BYTES[41 42 43 44 45 46 47 48]
        |    2/3: BYTES[41 42 43 44 45 46 47 48 ...]
        |    3/3: BYTES-STREAM[
        |        1: BYTES[41 42 43 44]
        |        2: BYTES[45 46 47 48]
        |        3: BYTES[49 4A 4B 4C]
        |        4: BYTES[4D 4E 4F 50]
        |        5: BYTES[51 52 53 54]
        |        6: BYTES[55 56 57 58]
        |        7: BYTES[59 5A]
        |    3/3: ]
        |1: ]
        |2: END
        |""".stripMargin
    }

    "text arrays" - roundTripLogEquals {
      ArrayElem.Sized(
        StringElem(alphabet.take(8)),
        StringElem(alphabet),
        TextStreamElem(alphabet.grouped(4).map(StringElem).toVector))
    } {
      """1: [
        |    1/3: "ABCDEFGH"
        |    2/3: "ABCDEFGH..."
        |    3/3: TEXT-STREAM[
        |        1: "ABCD"
        |        2: "EFGH"
        |        3: "IJKL"
        |        4: "MNOP"
        |        5: "QRST"
        |        6: "UVWX"
        |        7: "YZ"
        |    3/3: ]
        |1: ]
        |2: END
        |""".stripMargin
    }

    "arrays" - roundTripLogEquals {
      ArrayElem.Sized(
        ArrayElem.Sized(IntElem(1), IntElem(2), IntElem(3)),
        ArrayElem.Sized(),
        ArrayElem.Unsized(Vector(IntElem(1), IntElem(2), IntElem(3))))
    } {
      """1: [
        |    1/3: [
        |        1/3: 1
        |        2/3: 2
        |        3/3: 3
        |    1/3: ]
        |    2/3: []
        |    3/3: [
        |        1: 1
        |        2: 2
        |        3: 3
        |    3/3: ]
        |1: ]
        |2: END
        |""".stripMargin
    }

    "maps" - roundTripLogEquals {
      val tuples = "abc".map(c ⇒ c.toString → StringElem(c.toString))
      MapElem.Sized(
        "foo"   → IntElem(42),
        "empty" → MapElem.Sized(),
        "bar"   → MapElem.Unsized(tuples.head, tuples.tail: _*))
    } {
      """1: {
        |    1/3: "foo"
        |    1/3: -> 42
        |    2/3: "empty"
        |    2/3: -> {}
        |    3/3: "bar"
        |    3/3: -> {
        |        1: "a"
        |        1: -> "a"
        |        2: "b"
        |        2: -> "b"
        |        3: "c"
        |        3: -> "c"
        |    3/3: }
        |1: }
        |2: END
        |""".stripMargin
    }
  }

  def roundTripLogEquals(element: Element)(expectedLog: String): Unit = {
    val log = new java.lang.StringBuilder

    val encoded =
      Cbor
        .encode(element)
        .withStringLogging(log, maxShownByteArrayPrefixLen = 8, maxShownStringPrefixLen = 8)
        .toByteArray

    log.toString ==> expectedLog

    log.setLength(0) // clear
    Cbor
      .decode(encoded)
      .withStringLogging(log, maxShownByteArrayPrefixLen = 8, maxShownStringPrefixLen = 8)
      .to[Element]
      .value ==> element

    log.toString ==> expectedLog
  }
}
