/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import java.nio.charset.StandardCharsets

import io.bullet.borer._

class MiscSpec extends AbstractBorerSpec {

  def encode[T: Encoder](value: T): String   = Json.encode(value).toUtf8String
  def decode[T: Decoder](encoded: String): T = Json.decode(encoded getBytes StandardCharsets.UTF_8).to[T].value

  final case class CaseClass3(abc: Int, d: String, efghi: Boolean)

  final case class CaseClassT[T](key: String, value: T)

  final case class CaseClass1(flag: Boolean)

  object CaseClass1 {
    def apply(): CaseClass1 = new CaseClass1(false)
  }

  final case class CaseClass1T[T](value: T)

  test("Case Class with 3 members") {
    given codec: Codec[CaseClass3] = ArrayBasedCodecs.deriveCodec[CaseClass3]
    roundTrip("""[42,"",true]""", CaseClass3(42, "", efghi = true))
  }

  test("Generic Case Class with fixed codec") {
    given Codec[CaseClassT[Double]] = ArrayBasedCodecs.deriveCodec
    roundTrip("""["foo",18.1]""", CaseClassT("foo", 18.1))
  }

  test("Generic Case Class with generic codec") {
    given [T: Encoder: Decoder]: Codec[CaseClassT[T]] = ArrayBasedCodecs.deriveCodec
    roundTrip("""["foo",18.1]""", CaseClassT("foo", 18.1))
  }

  test("Unary Case Class with custom apply") {
    given Codec[CaseClass1] = ArrayBasedCodecs.deriveCodec
    roundTrip("false", CaseClass1(false))
  }

  test("Generic unary Case Class") {
    given [T: Encoder: Decoder]: Codec[CaseClass1T[T]] = ArrayBasedCodecs.deriveCodec
    roundTrip(""""foo"""", CaseClass1T("foo"))
  }

  test("Local Case Class") {
    case class Box(id: String)
    given Codec[Box] = ArrayBasedCodecs.deriveCodec[Box]
    roundTrip(""""abc"""", Box("abc"))
  }

  test("Recursive Case Class") {
    case class Box(x: Option[Box] = None)
    given Codec[Box] = ArrayBasedCodecs.deriveCodec[Box].recursive
    roundTrip("""[[[]]]""", Box(Some(Box(Some(Box())))))
  }

  test("<compact-map> - unary") {
    given Codec[CaseClass1] = CompactMapBasedCodecs.deriveCodec
    roundTrip("false", CaseClass1(false))
  }

  test("<compact-map> - non-unary") {
    given Codec[CaseClass3] = CompactMapBasedCodecs.deriveCodec
    roundTrip("""{"abc":42,"d":"","efghi":true}""", CaseClass3(42, "", efghi = true))
  }

  test("Missing map member with @key annotation") {
    case class Foo(x: Int, @key("z") y: String)

    given Codec[Foo] = MapBasedCodecs.deriveCodec
    val errorMsg     = """Cannot decode `Foo` instance due to missing map key "z" (input position 7)"""
    assertMatch(Json.decode("""{"x":42}""" getBytes StandardCharsets.UTF_8).to[Foo].valueEither) {
      case Left(e: Borer.Error.InvalidInputData[_]) if e.getMessage == errorMsg => // ok
    }
  }

  test("encodeCaseClassMemberDefaultValues = true") {
    case class Foo(x: Int, y: String = "bar");
    {
      given Codec[Foo] = MapBasedCodecs.deriveCodec
      verifyEncoding(Foo(42), """{"x":42}""")
    }
    {
      given DerivationConfig = DerivationConfig(encodeCaseClassMemberDefaultValues = true)
      given Codec[Foo]       = MapBasedCodecs.deriveCodec
      verifyEncoding(Foo(42), """{"x":42,"y":"bar"}""")
    }
  }

  test("Custom 'Unit' type") {
    sealed trait Unit
    given Decoder[Unit] = Decoder { r =>
      r.readNull()
      null
    }

    case class Foo(value: Double, unit: Unit)
    given Decoder[Foo] = MapBasedCodecs.deriveDecoder
    verifyDecoding("null", null)
  }

  test("Type alias") {
    type Foo = String
    case class Bar(bar: Foo)
    given Codec[Bar] = MapBasedCodecs.deriveCodec
    roundTrip("""{"bar":"yeah"}""", Bar("yeah"))
  }

  test("Dom Renderer") {
    import MapBasedCodecs.*
    case class Bar(stringSeq: Seq[String], double: Double) derives Encoder
    case class Foo(int: Int, bar: Bar, doubleOpt: Option[Double], boolOpt: Option[Boolean]) derives Encoder

    val foo = Foo(42, Bar(List("abc", "def"), 18.34), None, Some(true))

    val dom = Cbor.transEncode(foo).transDecode.to[Dom.Element].value

    dom.render() ==>
    """{
      |  "int" = 42,
      |  "bar" = {
      |    "stringSeq" = *[
      |      "abc",
      |      "def",
      |    ],
      |    "double" = 18.34d
      |  },
      |  "doubleOpt" = [],
      |  "boolOpt" = [
      |    true
      |  ]
      |}
      |""".stripMargin

    dom.render(renderLevelCount = true) ==>
    """    1| {
      |  1/4|   "int" = 42,
      |  2/4|   "bar" = {
      |  1/2|     "stringSeq" = *[
      |    1|       "abc",
      |    2|       "def",
      |  1/2|     ],
      |  2/2|     "double" = 18.34d
      |  2/4|   },
      |  3/4|   "doubleOpt" = [],
      |  4/4|   "boolOpt" = [
      |  1/1|     true
      |  4/4|   ]
      |    1| }
      |""".stripMargin
  }

  test("Extra members") {
    import MapBasedCodecs.*
    sealed trait Base derives Decoder.All
    case class A(x: Int) extends Base
    case class B()       extends Base
    case object C        extends Base

    verifyDecoding[Base]("""{"A":{"x":100, "extra": "should be ignored"}}""", A(100))
    verifyDecoding[Base]("""{"B":{"extra": "should be ignored"}}""", B())
    verifyDecoding[Base]("""{"C":{"extra": "should be ignored"}}""", C)
  }

  test("Problematic ADT") {
    val error = compileErrors("MapBasedCodecs.deriveAllEncoders[io.bullet.borer.derivation.BadAdt]")
    error.split('\n').slice(1, 3) ==> Array(
      "Could not find any sub-types of `BadAdt`, likely because `BadAdt` is not a fully closed ADT.",
      "Do you maybe have a `sealed trait` somewhere, which has no subclasses?"
    )
  }
}

sealed trait BadAdt
object BadAdt:
  case object A  extends BadAdt
  sealed trait B extends BadAdt
