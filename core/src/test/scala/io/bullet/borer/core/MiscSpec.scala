/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

import utest._

object MiscSpec extends BorerSpec.Default {

  case class Foo(int: Int, string: String, doubleOpt: Option[java.lang.Double])
  case class Bar(foo: Foo, optFoo: Option[Foo], stringSeq: Seq[String])

  implicit val fooCodec: Codec.Universal[Foo] = Codec.of[Foo].from(Foo.unapply, Foo.apply)
  implicit val barCodec: Codec.Universal[Bar] = Codec.of[Bar].from(Bar.unapply, Bar.apply)

  val tests = Tests {

    "basic roundtrip" - roundTrip(
      "838383182A63666F6F808183182B6081F93C00808383382B781DC3A17276C3AD7A74C5B172C59120C3BC74766566C3BA72C3B367C3A970" +
        "81FB403A2E147AE147AE8083616165627261766F647A756C7583831927106943424F5220726F787881FBC1678C29DCD6E9BC81830061" +
        "30808263796573626E6F",
      List(
        Bar(Foo(42, "foo", None), Some(Foo(43, "", Some(1.0))), Nil),
        Bar(Foo(-44, "árvíztűrő ütvefúrógép", Some(26.18)), None, Vector("a", "bravo", "zulu")),
        Bar(Foo(10000, "CBOR roxx", Some(-12345678.9012345)), Some(Foo(0, "0", None)), Vector("yes", "no"))
      )
    )

    "BigInts" - {
      roundTrip("00", BigInt(0))
      roundTrip("01", BigInt(1))
      roundTrip("20", BigInt(-1))
      roundTrip("1903E8", BigInt(1000))
      roundTrip("3903E7", BigInt(-1000))
      roundTrip("1A7FFFFFFF", BigInt(Int.MaxValue))
      roundTrip("3A7FFFFFFF", BigInt(Int.MinValue))
      roundTrip("1A80000000", BigInt(1L + Int.MaxValue))
      roundTrip("1A7FFFFFFF", BigInt(-1L - Int.MinValue))
      roundTrip("1B7FFFFFFFFFFFFFFF", BigInt(Long.MaxValue))
      roundTrip("3B7FFFFFFFFFFFFFFF", BigInt(Long.MinValue))
      roundTrip("1B8000000000000000", BigInt("9223372036854775808"))  // Long.MaxValue + 1
      roundTrip("3B8000000000000000", BigInt("-9223372036854775809")) // Long.MinValue - 1

      roundTrip(
        "C2581D748734B402B41DF49150F2D71EAA36FA06D63B69F95A89DA23E14FA668",
        BigInt("3141592653589793238462643383279502884197169399375105820974944592307816"))

      roundTrip(
        "C3581D748734B402B41DF49150F2D71EAA36FA06D63B69F95A89DA23E14FA667",
        BigInt("-3141592653589793238462643383279502884197169399375105820974944592307816"))
    }

    "BigDecimals" - {
      roundTrip("00", BigDecimal(0))
      roundTrip("C482010F", BigDecimal(1.5))
      roundTrip("C482012E", BigDecimal(-1.5))
      roundTrip("1903E8", BigDecimal(1000))
      roundTrip("C48201392714", BigDecimal(-1000.5))
      roundTrip("1A7FFFFFFF", BigDecimal(Int.MaxValue))
      roundTrip("3A7FFFFFFF", BigDecimal(Int.MinValue))
      roundTrip("1A80000000", BigDecimal(1L + Int.MaxValue))
      roundTrip("1A7FFFFFFF", BigDecimal(-1L - Int.MinValue))
      roundTrip("1B7FFFFFFFFFFFFFFF", BigDecimal(Long.MaxValue))
      roundTrip("3B7FFFFFFFFFFFFFFF", BigDecimal(Long.MinValue))
      roundTrip("1B8000000000000000", BigDecimal("9223372036854775808"))  // Long.MaxValue + 1
      roundTrip("3B8000000000000000", BigDecimal("-9223372036854775809")) // Long.MinValue - 1

      roundTrip(
        "C2581D748734B402B41DF49150F2D71EAA36FA06D63B69F95A89DA23E14FA668",
        BigDecimal("3141592653589793238462643383279502884197169399375105820974944592307816"))

      roundTrip("C48202191092", BigDecimal(42.42))
      roundTrip("C482061B00007048860F9180", BigDecimal(123456789.123456))

      roundTrip(
        "C4821845C2581D748734B402B41DF49150F2D71EAA36FA06D63B69F95A89DA23E14FA668",
        BigDecimal("3.141592653589793238462643383279502884197169399375105820974944592307816"))
    }
  }
}
