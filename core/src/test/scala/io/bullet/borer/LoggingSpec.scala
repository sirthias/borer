/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

class LoggingSpec extends BorerSuite:
  import Dom._

  val alphabet = ('A' to 'Z').mkString

  test("simple values") {
    roundTripLogEquals(
      ArrayElem.Sized(
        NullElem,
        UndefinedElem,
        BooleanElem(true),
        IntElem(42),
        LongElem(Int.MaxValue + 1L),
        OverLongElem(negative = false, -1),
        OverLongElem(negative = true, -1),
        Float16Elem(1.0f),
        FloatElem(100000.0f),
        DoubleElem(1.6),
        SimpleValueElem(SimpleValue(18))),
      initialGutterWidth = 7
    ) {
      """      1| [
        |   1/11|     null
        |   2/11|     undefined
        |   3/11|     true
        |   4/11|     42
        |   5/11|     2147483648L
        |   6/11|     18446744073709551615LL
        |   7/11|     -18446744073709551616LL
        |   8/11|     1.0f16
        |   9/11|     100000.0f
        |  10/11|     1.6d
        |  11/11|     Simple(18)
        |      1| ]
        |      2| END
        |""".stripMargin
    }
  }

  test("byte arrays") {
    roundTripLogEquals {
      ArrayElem.Sized(
        ByteArrayElem(alphabet.take(8).getBytes),
        ByteArrayElem(alphabet.getBytes),
        BytesStreamElem(alphabet.grouped(4).map(s => ByteArrayElem(s.getBytes)).toVector))
    } {
      """    1| [
        |  1/3|     B[41 42 43 44 45 46 47 48]
        |  2/3|     B[41 42 43 44 45 46 47 48 ...]
        |  3/3|     B*[
        |    1|         B[41 42 43 44]
        |    2|         B[45 46 47 48]
        |    3|         B[49 4A 4B 4C]
        |    4|         B[4D 4E 4F 50]
        |    5|         B[51 52 53 54]
        |    6|         B[55 56 57 58]
        |    7|         B[59 5A]
        |  3/3|     ]
        |    1| ]
        |    2| END
        |""".stripMargin
    }
  }

  test("text arrays") {
    roundTripLogEquals {
      ArrayElem.Sized(
        StringElem(alphabet.take(8)),
        StringElem(alphabet),
        TextStreamElem(alphabet.grouped(4).map(StringElem.apply).toVector))
    } {
      """    1| [
        |  1/3|     "ABCDEFGH"
        |  2/3|     "ABCDEFGH..."
        |  3/3|     T*[
        |    1|         "ABCD"
        |    2|         "EFGH"
        |    3|         "IJKL"
        |    4|         "MNOP"
        |    5|         "QRST"
        |    6|         "UVWX"
        |    7|         "YZ"
        |  3/3|     ]
        |    1| ]
        |    2| END
        |""".stripMargin
    }
  }

  test("arrays") {
    roundTripLogEquals {
      ArrayElem.Sized(
        ArrayElem.Sized(IntElem(1), IntElem(2), IntElem(3)),
        ArrayElem.Sized(),
        ArrayElem.Unsized(Vector(IntElem(1), IntElem(2), IntElem(3))))
    } {
      """    1| [
        |  1/3|     [
        |  1/3|         1
        |  2/3|         2
        |  3/3|         3
        |  1/3|     ]
        |  2/3|     []
        |  3/3|     *[
        |    1|         1
        |    2|         2
        |    3|         3
        |  3/3|     ]
        |    1| ]
        |    2| END
        |""".stripMargin
    }
  }

  test("maps") {
    roundTripLogEquals {
      val tuples = "abc".map(c => c.toString -> StringElem(c.toString))
      MapElem.Sized("foo" -> IntElem(42), "emp\nty" -> MapElem.Sized.empty, "bar" -> MapElem.Unsized(tuples: _*))
    } {
      """    1| {
        |  1/3|     "foo"
        |  1/3|     -> 42
        |  2/3|     "emp\nty"
        |  2/3|     -> {}
        |  3/3|     "bar"
        |  3/3|     -> *{
        |    1|         "a"
        |    1|         -> "a"
        |    2|         "b"
        |    2|         -> "b"
        |    3|         "c"
        |    3|         -> "c"
        |  3/3|     }
        |    1| }
        |    2| END
        |""".stripMargin
    }
  }

  test("long arrays and maps") {
    roundTripLogEquals {
      ArrayElem.Sized(
        ArrayElem.Sized((0 to 20).map(IntElem.apply): _*),
        MapElem.Sized((0 to 20).map(IntElem.apply).zip(('A' to 'Z').map(x => StringElem(x.toString))): _*)
      )
    } {
      """    1| [
        |  1/2|     [
        | 1/21|         0
        | 2/21|         1
        | 3/21|         2
        | 4/21|         3
        | 5/21|         4
        | 6/21|         5
        |        ...
        |16/21|         15
        |17/21|         16
        |18/21|         17
        |19/21|         18
        |20/21|         19
        |21/21|         20
        |  1/2|     ]
        |  2/2|     {
        | 1/21|         0
        | 1/21|         -> "A"
        | 2/21|         1
        | 2/21|         -> "B"
        | 3/21|         2
        | 3/21|         -> "C"
        | 4/21|         3
        | 4/21|         -> "D"
        | 5/21|         4
        | 5/21|         -> "E"
        | 6/21|         5
        | 6/21|         -> "F"
        | 7/21|         6
        | 7/21|         -> "G"
        | 8/21|         7
        | 8/21|         -> "H"
        |        ...
        |15/21|         14
        |15/21|         -> "O"
        |16/21|         15
        |16/21|         -> "P"
        |17/21|         16
        |17/21|         -> "Q"
        |18/21|         17
        |18/21|         -> "R"
        |19/21|         18
        |19/21|         -> "S"
        |20/21|         19
        |20/21|         -> "T"
        |21/21|         20
        |21/21|         -> "U"
        |  2/2|     }
        |    1| ]
        |    2| END
        |""".stripMargin
    }
  }

  def roundTripLogEquals(element: Element, initialGutterWidth: Int = 5)(expectedLog: String): Unit =
    val log = new java.lang.StringBuilder

    val encoded =
      Cbor
        .encode(element)
        .withStringLogging(
          log,
          maxShownByteArrayPrefixLen = 8,
          maxShownStringPrefixLen = 8,
          maxShownArrayElems = 12,
          maxShownMapEntries = 15,
          mapValueOnNewLine = true,
          initialGutterWidth = initialGutterWidth,
          lineSep = "\n")
        .toByteArray

    log.toString ==> expectedLog

    log.setLength(0) // clear
    Cbor
      .decode(encoded)
      .withStringLogging(
        log,
        maxShownByteArrayPrefixLen = 8,
        maxShownStringPrefixLen = 8,
        maxShownArrayElems = 12,
        maxShownMapEntries = 15,
        mapValueOnNewLine = true,
        initialGutterWidth = initialGutterWidth,
        lineSep = "\n")
      .to[Element]
      .value ==> element

    log.toString ==> expectedLog
