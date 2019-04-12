/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.math.BigInteger
import java.nio.charset.StandardCharsets.UTF_8

import io.bullet.borer.internal.Util
import utest._

import scala.collection.immutable.ListMap
import scala.util.Random

object JsonSpec extends BorerSpec {

  override def encode[T: Encoder](value: T): String = Json.encode(value).toUtf8String

  override def decode[T: Decoder](encoded: String): T = Json.decode(encoded getBytes UTF_8).to[T].value

  val tests = Tests {

    "Positive Ints" - {
      roundTrip("0", 0)
      roundTrip("1", 1)
      roundTrip("23", 23)
      roundTrip("345", 345)
      roundTrip("4567", 4567)
      roundTrip("78901", 78901)
      roundTrip("890123", 890123)
      roundTrip("1000000", 1000000)
      roundTrip("2147483647", Int.MaxValue)
      roundTrip("1000000000000", 1000000000000L)
      roundTrip("9223372036854775806", Long.MaxValue - 1)
      roundTrip("9223372036854775807", Long.MaxValue)
      roundTrip("9223372036854775808", new BigInteger("9223372036854775808"))
      roundTrip("9223372036854775809", new BigInteger("9223372036854775809"))
      roundTrip("18446744073709551615", new BigInteger("18446744073709551615"))
      roundTrip("18446744073709551616", new BigInteger("18446744073709551616"))
    }

    "Negative Ints" - {
      verifyDecoding("-0", 0)
      roundTrip("-1", -1)
      roundTrip("-23", -23)
      roundTrip("-345", -345)
      roundTrip("-4567", -4567)
      roundTrip("-78901", -78901)
      roundTrip("-890123", -890123)
      roundTrip("-1000000", -1000000)
      roundTrip("-2147483648", Int.MinValue)
      roundTrip("-1000000000000", -1000000000000L)
      roundTrip("-9223372036854775808", Long.MinValue)
      roundTrip("-18446744073709551616", new BigInteger("-18446744073709551616"))
      roundTrip("-18446744073709551617", new BigInteger("-18446744073709551617"))
    }

    "Floating Point Numbers" - {
      intercept[Borer.Error.InvalidJsonData[_ <: AnyRef]](encode(Float16(0.0f)))

      roundTrip("0.0", 0.0f)
      roundTrip("0.0", 0.0)

      if (Util.isJVM) {
        roundTrip("-0.0", -0.0f)
        roundTrip("-0.0", -0.0)
      }

      roundTrip("1.0", 1.0f)
      roundTrip("1.0", 1.0)

      roundTrip("1.1", 1.1)

      roundTrip("1.5", 1.5f)
      roundTrip("1.5", 1.5)

      roundTrip("65504.0", 65504.0f)
      roundTrip("65504.0", 65504.0)

      roundTrip("100000.0", 100000.0f)
      roundTrip("100000.0", 100000.0)

      if (Util.isJVM) {
        roundTrip("3.4028234663852886E38", 3.4028234663852886e+38f)
        roundTrip("3.4028234663852886E38", 3.4028234663852886e+38)

        roundTrip("1.0E300", 1.0e+300)

        roundTrip("6.103515625E-5", 0.00006103515625f)
        roundTrip("6.103515625E-5", 0.00006103515625)
      }

      roundTrip("-4.0", -4.0f)
      roundTrip("-4.0", -4.0)

      roundTrip("-4.1", -4.1)

      intercept[Borer.Error.InvalidJsonData[_ <: AnyRef]](encode(Double.NegativeInfinity))
      intercept[Borer.Error.InvalidJsonData[_ <: AnyRef]](encode(Double.PositiveInfinity))
      intercept[Borer.Error.InvalidJsonData[_ <: AnyRef]](encode(Double.NaN))
    }

    "Simple Values" - {
      roundTrip("false", false)
      roundTrip("true", true)
      roundTrip("null", null)
      intercept[Borer.Error.InvalidJsonData[_ <: AnyRef]](encode(Undefined))
      intercept[Borer.Error.InvalidJsonData[_ <: AnyRef]](encode(SimpleValue(16)))
    }

    "Tags, Byte Strings and Text Byte Strings" - {
      intercept[Borer.Error.InvalidJsonData[_ <: AnyRef]](encode[Tag](Tag.MagicHeader))
      intercept[Borer.Error.InvalidJsonData[_ <: AnyRef]](encode(Writer.Script(_.writeBytes(Array.emptyByteArray))))
      intercept[Borer.Error.InvalidJsonData[_ <: AnyRef]](encode(Writer.Script(_.writeBytesStart())))
      intercept[Borer.Error.InvalidJsonData[_ <: AnyRef]](encode(Writer.Script(_.writeText(Array.emptyByteArray))))
      intercept[Borer.Error.InvalidJsonData[_ <: AnyRef]](encode(Writer.Script(_.writeTextStart())))
    }

    "Strings" - {
      roundTrip("\"\"", "")
      roundTrip("\"foo\"", "foo")
      roundTrip(""""\"\\\b\f\n\r\t"""", "\"\\\b\f\n\r\t")
      roundTrip("\"\\u0000\"", "\u0000")                   // control char
      roundTrip("\"\\u0001\"", "\u0001")                   // control char
      roundTrip("\"\\u0010\"", "\u0010")                   // control char
      roundTrip("\"\\u001e\"", "\u001e")                   // control char
      roundTrip("\"\u007f\"", "\u007f")                    // 1-byte UTF-8 (normal ASCII char)
      roundTrip("\"\u00fc\"", "\u00fc")                    // 2-byte UTF-8
      roundTrip("\"\u6c34\"", "\u6c34")                    // 3-byte UTF-8
      roundTrip("\"\ud800\udd51\"", "\ud800\udd51")        // 4-byte UTF-8
      verifyDecoding("\"\\uDBFF\\uDFFF\"", "\uDBFF\uDFFF") // 4-byte UTF-8
      roundTrip("\"árvíztűrő ütvefúrógép\"", "árvíztűrő ütvefúrógép")
      roundTrip("\"飞机因此受到损伤\"", "飞机因此受到损伤")

      val strings = ('a' to 'z').mkString.inits.toList.init
      val all = for {
        escapes ← "abdgkpv".inits.toList.init
        str     ← strings
      } yield escapes.foldLeft(str)((s, c) ⇒ s.replace(c, '\n'))
      roundTrip(all.map(_.replace("\n", "\\n")).mkString("[\"", "\",\"", "\"]"), all)
    }

    "Arrays" - {
      intercept[Borer.Error.InvalidJsonData[_ <: AnyRef]](encode(Writer.Script(_.writeArrayHeader(0))))
      roundTrip("[]", List.empty[String])
      roundTrip("[1,2,3]", List(1, 2, 3))

      roundTrip("[1,[2,3],[4,5]]", (1, Vector(2, 3), List(4, 5)))
      roundTrip("[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]", 1 to 25: Iterable[Int])
    }

    "Maps" - {
      roundTrip("{}", Map.empty[Int, String])
      intercept[Borer.Error.UnexpectedDataItem[_ <: AnyRef]](encode(ListMap(1 → 2)))
      roundTrip("""{"":2,"foo":4}""", ListMap(""                   → 2, "foo"     → 4))
      roundTrip("""{"a":[[1],[]],"b":[[],[[2,3]]]}""", ListMap("a" → Left(1), "b" → Right(Vector(2, 3))))
      roundTrip("""[[[],["a"]],[[{"b":"c"}],[]]]""", Vector(Right("a"), Left(ListMap("b" → "c"))))
      roundTrip(
        """{"a":"A","b":"B","c":"C","d":"D","e":"E"}""",
        ListMap("a" → "A", "b" → "B", "c" → "C", "d" → "D", "e" → "E"))

      verifyDecoding(
        "{\"addr\":\"1x6YnuBVeeE65dQRZztRWgUPwyBjHCA5g\"\n}\n    ",
        ListMap("addr" → "1x6YnuBVeeE65dQRZztRWgUPwyBjHCA5g"))
    }

    "Whitespace" - {
      val wschars    = " \t\n\r"
      val random     = new Random()
      val wsCharIter = Iterator.continually(wschars.charAt(random.nextInt(wschars.length)))
      val wsStrings  = Iterator.continually(wsCharIter.take(random.nextInt(20)).mkString)
      def ws         = wsStrings.next()
      val list       = List(1, 2, 3)
      (1 to 100).foreach { _ ⇒
        verifyDecoding(s"$ws[${ws}1$ws,${ws}2$ws,${ws}3$ws]$ws", list)
      }
    }

    "Complex Case Classes" - {
      case class Foo(int: Int, string: String, doubleOpt: Option[java.lang.Double])
      case class Bar(foo: Foo, optFoo: Option[Foo], stringSeq: Seq[String])

      // we cannot use `Codec.deriveForCaseClass` since we are in the same compilation module
      implicit val fooCodec = Codec(Encoder.from(Foo.unapply _), Decoder.from(Foo.apply _))
      implicit val barCodec = Codec(Encoder.from(Bar.unapply _), Decoder.from(Bar.apply _))

      roundTrip(
        """[[[42,"foo",[]],[[43,"",[1.0]]],[]],[[-44,"árvíztűrő ütvefúrógép",[26.18]],[],["a","bravo","zulu"]],""" +
          """[[10000,"CBOR roxx",[-123.456]],[[0,"0",[]]],["yes","no"]]]""",
        Vector(
          Bar(Foo(42, "foo", None), Some(Foo(43, "", Some(1.0))), Nil),
          Bar(Foo(-44, "árvíztűrő ütvefúrógép", Some(26.18)), None, Vector("a", "bravo", "zulu")),
          Bar(Foo(10000, "CBOR roxx", Some(-123.456)), Some(Foo(0, "0", None)), Vector("yes", "no"))
        )
      )
    }

    "Zero-Member Case Class" - {
      case class Qux()
      implicit val quxCodec = Codec(Encoder.from(Qux.unapply _), Decoder.from(Qux.apply _))

      roundTrip("[]", Qux())
    }

    "Single-Member Case Class" - {
      case class Qux(i: Int)
      implicit val quxCodec = Codec(Encoder.from(Qux.unapply _), Decoder.from(Qux.apply _))

      roundTrip("42", Qux(42))
    }
  }
}
