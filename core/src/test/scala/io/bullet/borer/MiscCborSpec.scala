/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import io.bullet.borer.internal.Util.*

class MiscCborSpec extends ByteArrayCborSpec:

  case class Foo(int: Int, string: String, doubleOpt: Option[java.lang.Double])
  case class Bar(foo: Foo, optFoo: Option[Foo], stringSeq: Seq[String])

  // derivation makes this easy but we don't want to depend on it here
  implicit val fooCodec: Codec[Foo] = Codec.forProduct[Foo]
  implicit val barCodec: Codec[Bar] = Codec.forProduct[Bar]

  test("basic roundtrip")(
    roundTrip(
      "838383182a63666f6f808183182b6081f93c00808383382b781dc3a17276c3ad7a74c5b172c59120c3bc74766566c3ba72c3b367c3a970" +
        "81fb403a2e147ae147ae8083616165627261766f647a756c7583831927106943424f5220726f787881fbc1678c29dcd6e9bc81830061" +
        "30808263796573626e6f",
      Vector(
        Bar(Foo(42, "foo", None), Some(Foo(43, "", Some(1.0))), Nil),
        Bar(Foo(-44, "árvíztűrő ütvefúrógép", Some(26.18)), None, Vector("a", "bravo", "zulu")),
        Bar(Foo(10000, "CBOR roxx", Some(-12345678.9012345)), Some(Foo(0, "0", None)), Vector("yes", "no"))
      )
    ))

  case class Qux0()
  test("Zero-Member Case Class") {
    implicit val quxCodec: Codec[Qux0] = Codec.forProduct[Qux0]

    roundTrip("80", Qux0())
  }

  case class Qux1(i: Int)
  test("Single-Member Case Class") {
    implicit val quxCodec: Codec[Qux1] = Codec.forProduct[Qux1]
    roundTrip("182a", Qux1(42))
  }

  test("BigInts") {
    roundTrip("00", BigInt(0))
    roundTrip("01", BigInt(1))
    roundTrip("20", BigInt(-1))
    roundTrip("1903e8", BigInt(1000))
    roundTrip("3903e7", BigInt(-1000))
    roundTrip("1a7fffffff", BigInt(Int.MaxValue))
    roundTrip("3a7fffffff", BigInt(Int.MinValue))
    roundTrip("1a80000000", BigInt(1L + Int.MaxValue))
    roundTrip("1a7fffffff", BigInt(-1L - Int.MinValue))
    roundTrip("1b7fffffffffffffff", BigInt(Long.MaxValue))
    roundTrip("3b7fffffffffffffff", BigInt(Long.MinValue))
    roundTrip("1b8000000000000000", BigInt("9223372036854775808"))  // Long.MaxValue + 1
    roundTrip("3b8000000000000000", BigInt("-9223372036854775809")) // Long.MinValue - 1

    roundTrip(
      "c2581d748734b402b41df49150f2d71eaa36fa06d63b69f95a89da23e14fa668",
      BigInt("3141592653589793238462643383279502884197169399375105820974944592307816"))

    roundTrip(
      "c3581d748734b402b41df49150f2d71eaa36fa06d63b69f95a89da23e14fa667",
      BigInt("-3141592653589793238462643383279502884197169399375105820974944592307816"))

    // preferred serialization (RFC 8949 §3.4.3): no leading zero byte, even if the top bit of the magnitude is set
    roundTrip("c249ffffffffffffffffff", (BigInt(1) << 72) - 1)
    roundTrip("c349ffffffffffffffffff", -(BigInt(1) << 72))
    roundTrip("c249800000000000000000", BigInt(1) << 71)
    roundTrip("c349800000000000000000", -(BigInt(1) << 71) - 1)
    roundTrip("c24901ffffffffffffffff", (BigInt(1) << 65) - 1)
    roundTrip("c34901ffffffffffffffff", -(BigInt(1) << 65))
    roundTrip("c250ffffffffffffffffffffffffffffffff", (BigInt(1) << 128) - 1)
    roundTrip("c350ffffffffffffffffffffffffffffffff", -(BigInt(1) << 128))
  }

  test("BigDecimals") {
    roundTrip("00", BigDecimal(0))
    roundTrip("c482200f", BigDecimal(1.5))
    roundTrip("c482202e", BigDecimal(-1.5))
    roundTrip("1903e8", BigDecimal(1000))
    roundTrip("c48220392714", BigDecimal(-1000.5))
    roundTrip("1a7fffffff", BigDecimal(Int.MaxValue))
    roundTrip("3a7fffffff", BigDecimal(Int.MinValue))
    roundTrip("1a80000000", BigDecimal(1L + Int.MaxValue))
    roundTrip("1a7fffffff", BigDecimal(-1L - Int.MinValue))
    roundTrip("1b7fffffffffffffff", BigDecimal(Long.MaxValue))
    roundTrip("3b7fffffffffffffff", BigDecimal(Long.MinValue))
    roundTrip("1b8000000000000000", BigDecimal("9223372036854775808"))  // long.maxvalue + 1
    roundTrip("3b8000000000000000", BigDecimal("-9223372036854775809")) // long.minvalue - 1

    // preferred serialization (RFC 8949 §3.4.3) of the mantissa: no leading zero byte
    roundTrip("c249ffffffffffffffffff", BigDecimal((BigInt(1) << 72) - 1))
    roundTrip("c349ffffffffffffffffff", BigDecimal(-(BigInt(1) << 72)))

    roundTrip(
      "c2581d748734b402b41df49150f2d71eaa36fa06d63b69f95a89da23e14fa668",
      BigDecimal("3141592653589793238462643383279502884197169399375105820974944592307816"))

    roundTrip("c48221191092", BigDecimal(42.42))
    roundTrip("c482251b00007048860f9180", BigDecimal(123456789.123456))

    roundTrip(
      "c4823844c2581d748734b402b41df49150f2d71eaa36fa06d63b69f95a89da23e14fa668",
      BigDecimal("3.141592653589793238462643383279502884197169399375105820974944592307816"))

    // RFC 8949 §3.4.4: the value of a decimal fraction is `mantissa * 10^exponent`
    roundTrip("c48221196ab3", BigDecimal("273.15")) // the example from the RFC
    roundTrip("c482020f", BigDecimal("1.5E+3"))
    roundTrip("c48221c24901ffffffffffffffff", BigDecimal((BigInt(1) << 65) - 1, 2))
    roundTrip("c4823903e601", BigDecimal(BigInt(1), 999))
    verifyEncoding(BigDecimal(BigInt(1), Int.MaxValue), "c4823a7ffffffe01")
    verifyEncoding(BigDecimal(BigInt(1), Int.MinValue), "c4821a8000000001")
  }

  test("BigDecimal exponent limit") {
    intercept[Borer.Error.Overflow[_ <: AnyRef]](decode[BigDecimal]("c4823903e701"))     // exponent -1000
    intercept[Borer.Error.Overflow[_ <: AnyRef]](decode[BigDecimal]("c4821903e801"))     // exponent 1000
    intercept[Borer.Error.Overflow[_ <: AnyRef]](decode[BigDecimal]("c4823a7fffffff01")) // exponent Int.MinValue
  }

  test("BigDecimal mantissa limit") {
    // the configured limit applies to a plain bignum just like to the mantissa of a decimal fraction
    val maxEight = Decoder.forJBigDecimal(maxCborBigIntMantissaByteArraySize = 8)
    decode[java.math.BigDecimal]("c248ffffffffffffffff")(using maxEight) ==>
    new java.math.BigDecimal("18446744073709551615")
    intercept[Borer.Error.Overflow[_ <: AnyRef]](decode("c24901ffffffffffffffff")(using maxEight))
    intercept[Borer.Error.Overflow[_ <: AnyRef]](decode("c34901ffffffffffffffff")(using maxEight))
    intercept[Borer.Error.Overflow[_ <: AnyRef]](decode("c48221c24901ffffffffffffffff")(using maxEight))

    // also when it is above the default of 64 bytes
    val bignum65Bytes = "c25841" + "01" + "ff" * 64
    val max65         = Decoder.forJBigDecimal(maxCborBigIntMantissaByteArraySize = 65)
    decode[java.math.BigDecimal](bignum65Bytes)(using max65) ==>
    new java.math.BigDecimal(((BigInt(1) << 513) - 1).bigInteger)
    intercept[Borer.Error.Overflow[_ <: AnyRef]](decode[java.math.BigDecimal](bignum65Bytes))
  }

  test("Array-Header Mismatch Error") {
    val encoded = Cbor.encode(Writer.Script(_.writeArrayHeader(0))).toByteArray
    val error   = Cbor.decode(encoded).to[Foo].valueEither.swap.getOrElse(null)
    error.asInstanceOf[Borer.Error.InvalidInputData[_]].getMessage ==>
    "Expected Array-Header(3) but got Array-Header(0) (input position 0)"
  }

  test("Int Array") {
    roundTrip("8301182a190159", Array(1, 42, 345))
  }

  test("Byte Array") {
    verifyEncoding(hex"11223344", "4411223344")
    verifyDecoding("4411223344", hex"11223344")
    verifyDecoding("8411182218331844", hex"11223344")
  }

  test("Illegal Map Termination Error") {
    intercept[Borer.Error.UnexpectedEndOfInput[_]](
      encode(Writer.Script(_.writeMapHeader(2).writeInt(1).writeInt(2).writeInt(3)))
    )

    intercept[Borer.Error.InvalidInputData[_]](
      encode(Writer.Script(_.writeMapStart().writeInt(1).writeBreak()))
    ).getMessage ==> "Expected map entry value data item but got BREAK (Output.ToByteArray index 2)"
  }

  case class Qux2(bytes: Array[Byte], foo: Foo)

  test("Nested Encodings") {
    implicit val quxCodec: Codec[Qux2] = Codec[Qux2](
      encoder = { (w, x) =>
        w.writeArrayHeader(2)
          .writeTag(Tag.Other(2828))
          .writeBytes(x.bytes)
          .writeBytes(Cbor.encode(x.foo).withConfig(Cbor.EncodingConfig(allowBufferCaching = false)).toByteArray)
      },
      decoder = { r => ??? }
    )

    val qux = Qux2("YEAH".getBytes, Foo(42, "foo", None))
    verifyEncoding(qux, "82d90b0c44594541484883182a63666f6f80")
  }

  test("Issue 227") { // https://github.com/sirthias/borer/issues/227
    Cbor.decode(hex"80").to[Array[String]].value.getClass ==> classOf[Array[String]]
  }

  test("Raw writing") {
    val value  = Foo(42, "foo", Some(-1.2))
    val output = Output.ToByteArray(bufferSize = 1024, allowBufferCaching = true)
    val writer = Cbor.writer(output, Cbor.EncodingConfig(), Receiver.nopTransformer)
    writer.write(value)
    output.result() ==> hex"83182a63666f6f81fbbff3333333333333"
  }
