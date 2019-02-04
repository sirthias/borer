/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

import java.math.BigInteger
import scala.collection.immutable.{ListMap, TreeMap}
import utest._

/**
  * Direct implementation of https://tools.ietf.org/html/rfc7049#appendix-A
  */
abstract class AbstractRfcExamplesSpec[Bytes](testTypeName: String)(implicit byteAccess: ByteAccess[Bytes])
    extends BorerSpec[Bytes] {

  // for these test we need an alternative `Either` codec
  implicit def eitherEnc[A: Encoder.Universal, B: Encoder.Universal] =
    Encoder.of[Either[A, B]].from {
      case (w, Left(x))  ⇒ w.write(x)
      case (w, Right(x)) ⇒ w.write(x)
    }
  implicit def eitherDec[A: Decoder.Universal, B: Decoder.Universal] =
    Decoder.of[Either[A, B]].from { r ⇒
      r.tryRead[A]() match {
        case Some(x) ⇒ Left(x)
        case None    ⇒ Right(r[B])
      }
    }

  val tests = Tests {

    "Positive Ints" - {
      roundTrip("00", 0)
      roundTrip("01", 1)
      roundTrip("0a", 10)
      roundTrip("17", 23)
      roundTrip("1818", 24)
      roundTrip("1819", 25)
      roundTrip("1864", 100)
      roundTrip("1903e8", 1000)
      roundTrip("1a000f4240", 1000000)
      roundTrip("1b000000e8d4a51000", 1000000000000L)
      roundTrip("1bffffffffffffffff", new BigInteger("18446744073709551615"))
      roundTrip("c249010000000000000000", new BigInteger("18446744073709551616"))
    }

    "Negative Ints" - {
      roundTrip("3bffffffffffffffff", new BigInteger("-18446744073709551616"))
      roundTrip("c349010000000000000000", new BigInteger("-18446744073709551617"))
      roundTrip("20", -1)
      roundTrip("29", -10)
      roundTrip("3863", -100)
      roundTrip("3903e7", -1000)
    }

    "Floating Point Numbers" - {
      roundTrip("f90000", Float16(0.0f))
      roundTrip("f90000", 0.0f)
      roundTrip("f90000", 0.0)

      roundTrip("f98000", Float16(-0.0f))
      roundTrip("f98000", -0.0f)
      roundTrip("f98000", -0.0)

      roundTrip("f93c00", Float16(1.0f))
      roundTrip("f93c00", 1.0f)
      roundTrip("f93c00", 1.0)

      roundTrip("fb3ff199999999999a", 1.1)

      roundTrip("f93e00", Float16(1.5f))
      roundTrip("f93e00", 1.5f)
      roundTrip("f93e00", 1.5)

      roundTrip("f97bff", Float16(65504.0f))
      roundTrip("f97bff", 65504.0f)
      roundTrip("f97bff", 65504.0)

      roundTrip("fa47c35000", 100000.0f)
      roundTrip("fa47c35000", 100000.0)

      roundTrip("fa7f7fffff", 3.4028234663852886e+38f)
      roundTrip("fa7f7fffff", 3.4028234663852886e+38)

      roundTrip("fb7e37e43c8800759c", 1.0e+300)

      roundTrip("f90001", Float16(5.960464477539063e-8f))

      roundTrip("f90400", Float16(0.00006103515625f))
      roundTrip("f90400", 0.00006103515625f)
      roundTrip("f90400", 0.00006103515625)

      roundTrip("f9c400", Float16(-4.0f))
      roundTrip("f9c400", -4.0f)
      roundTrip("f9c400", -4.0)

      roundTrip("fbc010666666666666", -4.1)

      roundTrip("f97c00", Float16(Float.PositiveInfinity))
      roundTrip("f97c00", Float.PositiveInfinity)
      roundTrip("f97c00", Double.PositiveInfinity)

      roundTrip("f97e00", Float16(Float.NaN))
      roundTrip("f97e00", Float.NaN)
      roundTrip("f97e00", Double.NaN)

      roundTrip("f9fc00", Float16(Float.NegativeInfinity))
      roundTrip("f9fc00", Float.NegativeInfinity)
      roundTrip("f9fc00", Double.NegativeInfinity)

      decode("fa7f800000", Float.PositiveInfinity)
      decode("fa7fc00000", Float.NaN)
      decode("faff800000", Float.NegativeInfinity)

      decode("fb7ff0000000000000", Double.PositiveInfinity)
      decode("fb7ff8000000000000", Double.NaN)
      decode("fbfff0000000000000", Double.NegativeInfinity)
    }

    "Simple Values" - {
      roundTrip("f4", false)
      roundTrip("f5", true)
      roundTrip("f6", null)
      roundTrip("f7", Undefined)
      roundTrip("f0", SimpleValue(16))
      roundTrip("f818", SimpleValue(24))
      roundTrip("f8ff", SimpleValue(255))
    }

    "Tagged Values" - {
      roundTrip("c074323031332d30332d32315432303a30343a30305a", Tag.DateTimeString @@ "2013-03-21T20:04:00Z")
      roundTrip("c11a514b67b0", Tag.EpochDateTime @@ 1363896240)
      roundTrip("c1fb41d452d9ec200000", Tag.EpochDateTime @@ 1363896240.5)
      roundTrip("d74401020304", Tag.HintBase16 @@ hexBytes("01020304"))
      roundTrip("d818456449455446", Tag.EmbeddedCBOR @@ hexBytes("6449455446"))
      roundTrip("d82076687474703a2f2f7777772e6578616d706c652e636f6d", Tag.TextUri @@ "http://www.example.com")
    }

    "Byte Strings" - {
      roundTrip("40", Array.emptyByteArray)
      roundTrip("4401020304", hexBytes("01020304"))
    }

    "Text Strings" - {
      roundTrip("60", "")
      roundTrip("6161", "a")
      roundTrip("6449455446", "IETF")
      roundTrip("62225c", "\"\\")
      roundTrip("62c3bc", "\u00fc")
      roundTrip("63e6b0b4", "\u6c34")
      roundTrip("64f0908591", "\ud800\udd51")
    }

    "Arrays" - {
      roundTrip("80", Array.empty[String])
      roundTrip("83010203", List(1, 2, 3))
      roundTrip("8301820203820405", (1, List(2, 3), List(4, 5)))
      roundTrip("98190102030405060708090a0b0c0d0e0f101112131415161718181819", (1 to 25).toVector)
    }

    "Maps" - {
      roundTrip("a0", TreeMap.empty[Int, String])
      roundTrip("a201020304", TreeMap(1           → 2, 3         → 4))
      roundTrip("a26161016162820203", TreeMap("a" → Left(1), "b" → Right(List(2, 3))))
      roundTrip("826161a161626163", List(Right("a"), Left(TreeMap("b" → "c"))))
      roundTrip(
        "a56161614161626142616361436164614461656145",
        TreeMap("a" → "A", "b" → "B", "c" → "C", "d" → "D", "e" → "E"))
    }

    "Unbounded Data Items" - {
      import Writer.Script._

      roundTrip("5f42010243030405ff", hexBytes("0102030405"), Iterator(hexBytes("0102"), hexBytes("030405")))

      roundTrip("7f657374726561646d696e67ff", "streaming", Iterator("strea", "ming"))

      roundTrip("9fff", List.empty[Int], Iterator.empty: Iterator[Int])

      roundTrip(
        "9f018202039f0405ffff",
        (1, List(2, 3), List(4, 5)),
        Writer.script(_ ~ ArrayStart ~ 1 ~ List(2, 3) ~ Iterator(4, 5) ~ Break))

      roundTrip(
        "9f01820203820405ff",
        (1, List(2, 3), List(4, 5)),
        Writer.script(_ ~ ArrayStart ~ 1 ~ List(2, 3) ~ List(4, 5) ~ Break))

      roundTrip("83018202039f0405ff", (1, List(2, 3), List(4, 5)), (1, List(2, 3), Iterator(4, 5)))

      roundTrip("83019f0203ff820405", (1, List(2, 3), List(4, 5)), (1, Iterator(2, 3), List(4, 5)))

      roundTrip("9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff", (1 to 25).toVector, (1 to 25).iterator)

      roundTrip(
        "bf61610161629f0203ffff",
        TreeMap("a" → Left(1), "b" → Right(List(2, 3))),
        Writer.script(_ ~ MapStart ~ "a" ~ 1 ~ "b" ~ Iterator(2, 3) ~ Break))

      roundTrip(
        "826161bf61626163ff",
        List(Left("a"), Right(TreeMap("b" → "c"))),
        "a" → Writer.script(_ ~ MapStart ~ "b" ~ "c" ~ Break))

      roundTrip(
        "bf6346756ef563416d7421ff",
        ListMap("Fun" → Left(true), "Amt" → Right(-2)),
        Writer.script(_ ~ MapStart ~ "Fun" ~ true ~ "Amt" ~ -2 ~ Break))
    }
  }
}
