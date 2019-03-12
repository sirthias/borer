/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import scala.collection.immutable.ListMap
import utest._

// JVM-specific due to the not platform-independent way of Float/Double toStrings
// see: https://www.scala-js.org/doc/semantics.html
object LoggingSpec extends TestSuite {
  import Dom.Element
  import Dom.Element.Value

  val alphabet = ('A' to 'Z').mkString

  val tests = Tests {

    "simple values" - roundTripLogEquals {
      Element.Array(
        Value.Null,
        Value.Undefined,
        Value.Bool(true),
        Value.Int(42),
        Value.Long(Int.MaxValue + 1L),
        Value.OverLong(negative = false, -1),
        Value.OverLong(negative = true, -1),
        Value.Float16(1.0f),
        Value.Float(100000.0f),
        Value.Double(1.6),
        Value.Simple(SimpleValue(18)))
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
      Element.Array(
        Value.ByteArray(alphabet.take(8).getBytes),
        Value.ByteArray(alphabet.getBytes),
        Value.BytesStream(alphabet.grouped(4).map(s ⇒ Value.ByteArray(s.getBytes)).toVector))
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
      Element.Array(
        Value.String(alphabet.take(8)),
        Value.String(alphabet),
        Value.TextStream(alphabet.grouped(4).map(Value.String).toVector))
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
      Element.Array(
        Element.Array(Value.Int(1), Value.Int(2), Value.Int(3)),
        Element.Array(),
        Element.Array(Vector(Value.Int(1), Value.Int(2), Value.Int(3)), indefiniteLength = true))
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
      val tuples = "abc".map { c ⇒
        val x = Element.Value.String(c.toString); x → x
      }
      Element.Map(
        "foo"   → Element.Value.Int(42),
        "empty" → Element.Map(),
        "bar"   → Element.Map(ListMap(tuples: _*), indefiniteLength = true))
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
