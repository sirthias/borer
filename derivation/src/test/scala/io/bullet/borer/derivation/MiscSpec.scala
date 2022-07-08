/*
 * Copyright (c) 2019-2022 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import java.nio.charset.StandardCharsets

import io.bullet.borer._
import utest._

object MiscSpec extends AbstractBorerSpec {

  def encode[T: Encoder](value: T): String   = Json.encode(value).toUtf8String
  def decode[T: Decoder](encoded: String): T = Json.decode(encoded getBytes StandardCharsets.UTF_8).to[T].value

  final case class CaseClass3(abc: Int, d: String, efghi: Boolean)

  final case class CaseClassT[T](key: String, value: T)

  final case class CaseClass1(flag: Boolean)

  object CaseClass1 {
    def apply(): CaseClass1 = new CaseClass1(false)
  }

  final case class CaseClass1T[T](value: T)

  val tests = Tests {

    "Case Class with 3 members" - {
      implicit val codec = ArrayBasedCodecs.deriveCodec[CaseClass3]
      roundTrip("""[42,"",true]""", CaseClass3(42, "", efghi = true))
    }

    "Generic Case Class with fixed codec" - {
      implicit val codec = ArrayBasedCodecs.deriveCodec[CaseClassT[Double]]
      roundTrip("""["foo",18.1]""", CaseClassT("foo", 18.1))
    }

    "Generic Case Class with generic codec" - {
      implicit def codec[T: Encoder: Decoder]: Codec[CaseClassT[T]] = ArrayBasedCodecs.deriveCodec[CaseClassT[T]]
      roundTrip("""["foo",18.1]""", CaseClassT("foo", 18.1))
    }

    "Unary Case Class with custom apply" - {
      implicit val codec = ArrayBasedCodecs.deriveCodec[CaseClass1]
      roundTrip("false", CaseClass1(false))
    }

    "Generic unary Case Class" - {
      implicit def codec[T: Encoder: Decoder]: Codec[CaseClass1T[T]] = ArrayBasedCodecs.deriveCodec[CaseClass1T[T]]
      roundTrip(""""foo"""", CaseClass1T("foo"))
    }

    "Local Case Class" - {
      case class Box(id: String)
      implicit val boxCodec = ArrayBasedCodecs.deriveCodec[Box]
      roundTrip(""""abc"""", Box("abc"))
    }

    "Recursive Case Class" - {
      case class Box(x: Option[Box] = None)
      implicit lazy val codec: Codec[Box] = ArrayBasedCodecs.deriveCodec[Box]
      roundTrip("""[[[]]]""", Box(Some(Box(Some(Box())))))
    }

    "CompactMapBasedCodecs" - {

      "unary" - {
        implicit val codec = CompactMapBasedCodecs.deriveCodec[CaseClass1]
        roundTrip("false", CaseClass1(false))
      }

      "non-unary" - {
        implicit val codec = CompactMapBasedCodecs.deriveCodec[CaseClass3]
        roundTrip("""{"abc":42,"d":"","efghi":true}""", CaseClass3(42, "", efghi = true))
      }
    }

    "Missing map member with @key annotation" - {
      final case class Foo(x: Int, @key("z") y: String)

      implicit val codec = MapBasedCodecs.deriveCodec[Foo]
      val errorMsg       = """Cannot decode `Foo` instance due to missing map key "z" (input position 7)"""
      assertMatch(Json.decode("""{"x":42}""" getBytes StandardCharsets.UTF_8).to[Foo].valueEither) {
        case Left(e: Borer.Error.InvalidInputData[_]) if e.getMessage == errorMsg => // ok
      }
    }

    "encodeCaseClassMemberDefaultValues = true" - {
      final case class Foo(x: Int, y: String = "bar");
      {
        implicit val codec = MapBasedCodecs.deriveCodec[Foo]
        verifyEncoding(Foo(42), """{"x":42}""")
      }
      {
        implicit val myConfig = DerivationConfig(encodeCaseClassMemberDefaultValues = true)
        implicit val codec    = MapBasedCodecs.deriveCodec[Foo]
        verifyEncoding(Foo(42), """{"x":42,"y":"bar"}""")
      }
    }

    "Custom 'Unit' type" - {
      sealed trait Unit
      implicit val unitDecoder: Decoder[Unit] = Decoder { r => r.readNull(); null }

      final case class Foo(value: Double, unit: Unit)
      implicit val fooDecoder: Decoder[Foo] = MapBasedCodecs.deriveDecoder
      verifyDecoding("null", null)
    }

    "Type alias" - {
      type Foo = String
      case class Bar(bar: Foo)
      implicit val barCodec: Codec[Bar] = MapBasedCodecs.deriveCodec
      roundTrip("""{"bar":"yeah"}""", Bar("yeah"))
    }

    "Dom Renderer" - {
      case class Bar(stringSeq: Seq[String], double: Double)
      case class Foo(int: Int, bar: Bar, doubleOpt: Option[Double], boolOpt: Option[Boolean])

      implicit val barEncoder: Encoder[Bar] = MapBasedCodecs.deriveEncoder
      implicit val fooEncoder: Encoder[Foo] = MapBasedCodecs.deriveEncoder

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
  }
}
