/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.math.BigInteger

import io.bullet.borer.internal.Util._
import utest._

import scala.collection.immutable.ListMap
import scala.util.Random

abstract class AbstractJsonSuiteSpec extends AbstractBorerSpec {

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
      intercept[Borer.Error.Unsupported[_ <: AnyRef]](encode(Float16(0.0f)))

      roundTrip("0.0", 0.0f)
      roundTrip("0.0", 0.0)

      roundTrip("1.5", 1.5f)
      roundTrip("1.5", 1.5)

      roundTrip("65504.0", 65504.0f)
      roundTrip("65504.0", 65504.0)

      roundTrip("100000.0", 100000.0f)
      roundTrip("100000.0", 100000.0)

      if (isJVM) {
        roundTrip("-0.0", -0.0f)
        roundTrip("-0.0", -0.0)

        decode[Double]("0.0").toString ==> "0.0"

        roundTrip("1.1999999", 1.1999999f)

        roundTrip("3.4028234663852886E38", 3.4028234663852886e+38)

        roundTrip("1.0E300", 1.0e+300)

        roundTrip("[2.10233856E8]", List(2.10233856e8f))
      }

      roundTrip("-4.0", -4.0f)
      roundTrip("-4.0", -4.0)

      roundTrip("-4.1", -4.1)

      intercept[Borer.Error.Unsupported[_ <: AnyRef]](encode(Double.NegativeInfinity))
      intercept[Borer.Error.Unsupported[_ <: AnyRef]](encode(Double.PositiveInfinity))
      intercept[Borer.Error.Unsupported[_ <: AnyRef]](encode(Double.NaN))

      intercept[Borer.Error.Overflow[_ <: AnyRef]](decode[Long]("1234567890123456789012345678901234567890"))
      intercept[Borer.Error.Overflow[_ <: AnyRef]](decode[Long]("12345.67890123456789012345678901234567890"))
      intercept[Borer.Error.Overflow[_ <: AnyRef]](decode[Long]("1E400"))
      intercept[Borer.Error.Overflow[_ <: AnyRef]](decode[Long]("1E400"))
    }

    "General Number Parsing" - {
      verifyDecoding("0", Dom.IntElem(0))
      verifyDecoding("0.0", Dom.DoubleElem(0))

      verifyDecoding("1234567890123456789012", Dom.NumberStringElem("1234567890123456789012"))
      verifyDecoding("123456789012345678901", Dom.NumberStringElem("123456789012345678901"))
      verifyDecoding("12345678901234567890", Dom.NumberStringElem("12345678901234567890"))
      verifyDecoding("1234567890123456789", Dom.LongElem(1234567890123456789L))
      verifyDecoding("0.1234567890123456789", Dom.NumberStringElem("0.1234567890123456789"))
      verifyDecoding("1.234567890123456789", Dom.NumberStringElem("1.234567890123456789"))
      verifyDecoding("12.34567890123456789", Dom.NumberStringElem("12.34567890123456789"))
      verifyDecoding("123.4567890123456789", Dom.NumberStringElem("123.4567890123456789"))
      verifyDecoding("1234567890123456.789", Dom.NumberStringElem("1234567890123456.789"))
      verifyDecoding("12345678901234567.89", Dom.NumberStringElem("12345678901234567.89"))
      verifyDecoding("123456789012345678.9", Dom.NumberStringElem("123456789012345678.9"))
      verifyDecoding("1234567890123456789.0", Dom.NumberStringElem("1234567890123456789.0"))

      verifyDecoding("123456789012345678", Dom.LongElem(123456789012345678L))
      verifyDecoding("1.23456789012345678", Dom.NumberStringElem("1.23456789012345678"))
      verifyDecoding("1.2345678901234567", Dom.NumberStringElem("1.2345678901234567"))
      verifyDecoding("1.234567890123456", Dom.DoubleElem(1.234567890123456))
      verifyDecoding("123456789012345.678", Dom.NumberStringElem("123456789012345.678"))
      verifyDecoding("123456789012345.67", Dom.NumberStringElem("123456789012345.67"))
      verifyDecoding("123456789012345.6", Dom.DoubleElem(123456789012345.6))

      verifyDecoding("1234567890123", Dom.LongElem(1234567890123L))
      verifyDecoding("1.234567890123", Dom.DoubleElem(1.234567890123))
      verifyDecoding("12.34567890123", Dom.DoubleElem(12.34567890123))
      verifyDecoding("123.4567890123", Dom.DoubleElem(123.4567890123))
      verifyDecoding("1234567890.123", Dom.DoubleElem(1234567890.123))
      verifyDecoding("12345678901.23", Dom.DoubleElem(12345678901.23))
      verifyDecoding("123456789012.3", Dom.DoubleElem(123456789012.3))
      verifyDecoding("1234567890123.0", Dom.DoubleElem(1.234567890123e12))
      verifyDecoding("1234567890123.00", Dom.DoubleElem(1.234567890123e12))

      verifyDecoding("12345678901234E0", Dom.LongElem(12345678901234L))
      verifyDecoding("12345678901234E+0", Dom.LongElem(12345678901234L))
      verifyDecoding("12345678901234E1", Dom.LongElem(123456789012340L))
      verifyDecoding("12345678901234E-1", Dom.DoubleElem(1234567890123.4))

      verifyDecoding("123456789012345E24", Dom.NumberStringElem("123456789012345E24"))
      verifyDecoding("123456789012345E23", Dom.NumberStringElem("123456789012345E23"))
      verifyDecoding("123456789012345E20", Dom.DoubleElem(1.23456789012345e34))
      verifyDecoding("123456789012345E19", Dom.DoubleElem(1.23456789012345e33))
      verifyDecoding("123456789012345E6", Dom.DoubleElem(1.23456789012345e20))
      verifyDecoding("123456789012345E5", Dom.DoubleElem(1.23456789012345e19))
      verifyDecoding("123456789012345E4", Dom.LongElem(1234567890123450000L))
      verifyDecoding("123456789012345E3", Dom.LongElem(123456789012345000L))
      verifyDecoding("123456789012345E2", Dom.LongElem(12345678901234500L))
      verifyDecoding("123456789012345E1", Dom.LongElem(1234567890123450L))
      verifyDecoding("123456789012345E0", Dom.LongElem(123456789012345L))
      verifyDecoding("123456789012345E-1", Dom.DoubleElem(12345678901234.5))
      verifyDecoding("123456789012345E-2", Dom.DoubleElem(1234567890123.45))
      verifyDecoding("123456789012345E-14", Dom.DoubleElem(1.23456789012345))
      verifyDecoding("123456789012345E-15", Dom.DoubleElem(0.123456789012345))
      verifyDecoding("123456789012345E-20", Dom.DoubleElem(1.23456789012345e-6))
      verifyDecoding("123456789012345E-21", Dom.DoubleElem(1.23456789012345e-7))
      verifyDecoding("123456789012345E-22", Dom.DoubleElem(1.23456789012345e-8))
      verifyDecoding("123456789012345E-23", Dom.NumberStringElem("123456789012345E-23"))
      verifyDecoding("123456789012345E-24", Dom.NumberStringElem("123456789012345E-24"))

      verifyDecoding("1.000000000000000", Dom.DoubleElem(1.0f))
      verifyDecoding("1.0000000000000000", Dom.NumberStringElem("1.0000000000000000"))

      verifyDecoding("1234567890123456789012e15", Dom.NumberStringElem("1234567890123456789012e15"))
      verifyDecoding("1234567890123456789012e+5", Dom.NumberStringElem("1234567890123456789012e+5"))
      verifyDecoding("1234567890123456789012e-1", Dom.NumberStringElem("1234567890123456789012e-1"))

      verifyDecoding("1", 1.0f)

      verifyDecoding("1.234", Dom.DoubleElem(1.234))
      Json
        .decode("1.234".getBytes)
        .withConfig(Json.DecodingConfig.default.copy(readDecimalNumbersOnlyAsNumberStrings = true))
        .to[Dom.Element]
        .value ==> Dom.NumberStringElem("1.234")
    }

    "Number Exercise" - {
      val numbers =
        for {
          number  <- "123456789123456789123456".tails.toList.init
          ix      <- (-1 until number.length).toList
          trailer <- List("", " ")
        } yield ix match {
          case -1                     => number + trailer
          case 0                      => "0." + number + trailer
          case x if x < number.length => number.substring(0, x) + "." + number.substring(x)
        }
      val json0                                = numbers.mkString("[", ",", "]")
      val dom0 @ Dom.ArrayElem.Unsized(elems0) = decode[Dom.Element](json0).asInstanceOf[Dom.ArrayElem.Unsized]
      val json1                                = encode(dom0)
      val dom1 @ Dom.ArrayElem.Unsized(elems1) = decode[Dom.Element](json1).asInstanceOf[Dom.ArrayElem.Unsized]
      elems0.zip(elems1).zipWithIndex.find { case ((a, b), _) => a != b } ==> None
    }

    "Simple Values" - {
      roundTrip("false", false)
      roundTrip("true", true)
      roundTrip("null", null)
      intercept[Borer.Error.Unsupported[_ <: AnyRef]](encode(Undefined))
      intercept[Borer.Error.Unsupported[_ <: AnyRef]](encode(SimpleValue(16)))
    }

    "Tags, Byte Strings and Text Byte Strings" - {
      intercept[Borer.Error.Unsupported[_ <: AnyRef]](encode[Tag](Tag.MagicHeader))
      intercept[Borer.Error.Unsupported[_ <: AnyRef]](encode(Writer.Script(_.writeBytes(Array.emptyByteArray))))
      intercept[Borer.Error.Unsupported[_ <: AnyRef]](encode(Writer.Script(_.writeBytesStart())))
      intercept[Borer.Error.Unsupported[_ <: AnyRef]](encode(Writer.Script(_.writeText(Array.emptyByteArray))))
      intercept[Borer.Error.Unsupported[_ <: AnyRef]](encode(Writer.Script(_.writeTextStart())))
    }

    "Strings" - {
      roundTrip("\"\"", "")
      roundTrip("\"foo\"", "foo")
      roundTrip(""""\"\\\b\r\f\r\n\n\t"""", "\"\\\b\r\f\r\n\n\t")
      roundTrip(""""foo\r\nbar\r\n"""", "foo\r\nbar\r\n")
      roundTrip("\"\\u0000\"", "\u0000")                   // control char
      roundTrip("\"\\u0001\"", "\u0001")                   // control char
      roundTrip("\"\\u0010\"", "\u0010")                   // control char
      roundTrip("\"\\u001e\"", "\u001e")                   // control char
      roundTrip("\"\u007f\"", "\u007f")                    // 1-byte UTF-8 (normal ASCII char)
      roundTrip("\"\u00fc\"", "\u00fc")                    // 2-byte UTF-8
      roundTrip("\"\u6c34\"", "\u6c34")                    // 3-byte UTF-8
      roundTrip("\"\ud800\udd51\"", "\ud800\udd51")        // 4-byte UTF-8
      verifyDecoding("\"\\uDBFF\\uDFFF\"", "\uDBFF\uDFFF") // 4-byte UTF-8
      roundTrip("\"Hällo stränger!\"", "Hällo stränger!")
      roundTrip("\"árvíztűrő ütvefúrógép\"", "árvíztűrő ütvefúrógép")
      roundTrip("\"เอส เอฟ ซีเนม่า เอ็มบีเค เซ็นเตอร์\"", "เอส เอฟ ซีเนม่า เอ็มบีเค เซ็นเตอร์")
      roundTrip("\"飞机因此受到损伤\"", "飞机因此受到损伤")
      roundTrip(
        /**/ "\"เอสds飞机hu เอฟ到a ซ'ีเ$นม่า เอ็#ม损บีเ00因0ค เซ็นเbตอร์\"",
        /*  */ "เอสds飞机hu เอฟ到a ซ'ีเ$นม่า เอ็#ม损บีเ00因0ค เซ็นเbตอร์")

      intercept[Borer.Error.InvalidInputData[_ <: AnyRef]] {
        Json.decode(hex"22dd1dd83422").to[String].value ==> "xxx"
      }.getMessage ==> "Illegal UTF-8 character encoding (input position 1)"

      val strings = ('a' to 'z').mkString.inits.toList.init
      val all = for {
        escapes <- "abdgkpv".inits.toList.init
        str     <- strings
      } yield escapes.foldLeft(str)((s, c) => s.replace(c, '\n'))
      roundTrip(all.map(_.replace("\n", "\\n")).mkString("[\"", "\",\"", "\"]"), all)
    }

    "Arrays" - {
      intercept[Borer.Error.Unsupported[_ <: AnyRef]](encode(Writer.Script(_.writeArrayHeader(0))))
      roundTrip("[]", List.empty[String])
      roundTrip("[1,2,3]", Array(1, 2, 3))

      roundTrip("[1,[2,3],[4,5]]", (1, Vector(2, 3), List(4, 5)))
      roundTrip("[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]", 1 to 25: Iterable[Int])

      roundTrip("[9223372036854775807,-9223372036854775808]", Array(Long.MaxValue, Long.MinValue))
      roundTrip("[3326704554664441231]", Array(BigInt("3326704554664441231")))
      roundTrip("[3326704554664441231]", Array(BigDecimal("3326704554664441231")))
    }

    "Maps" - {
      import Codec.ForEither.default

      roundTrip("{}", Map.empty[Int, String])
      roundTrip("""{"":2,"foo":4}""", ListMap("" -> 2, "foo" -> 4))
      roundTrip("""{"a":[0,1],"b":[1,[2,3]]}""", ListMap("a" -> Left(1), "b" -> Right(Vector(2, 3))))
      roundTrip("""[[1,"a"],[0,{"b":"c"}]]""", Vector(Right("a"), Left(ListMap("b" -> "c"))))
      roundTrip(
        """{"a":"A","b":"B","c":"C","d":"D","e":"E"}""",
        ListMap("a" -> "A", "b" -> "B", "c" -> "C", "d" -> "D", "e" -> "E"))

      verifyDecoding(
        "{\"addr\":\"1x6YnuBVeeE65dQRZztRWgUPwyBjHCA5g\"\n}\n    ",
        ListMap("addr" -> "1x6YnuBVeeE65dQRZztRWgUPwyBjHCA5g"))
    }

    "Maps with numeric keys" - {
      verifyEncoding(ListMap(1 -> 2, 2 -> 4), """{"1":2,"2":4}""")

      case class Maps(
          intKey: ListMap[Int, String]
      )

      implicit val mapsCodec = Codec(Encoder.from(Maps.unapply _), Decoder.from(Maps.apply _))

      val map = Maps(
        intKey = ListMap(1 -> "Int")
      )

      roundTrip("""{"1":"Int"}""", map)
    }

    "Whitespace" - {
      val wschars    = " \t\n\r"
      val random     = new Random()
      val wsCharIter = Iterator.continually(wschars.charAt(random.nextInt(wschars.length)))
      val wsStrings  = Iterator.continually(wsCharIter.take(random.nextInt(20)).mkString)
      def ws         = wsStrings.next()
      val list       = List(1, 2, 3)
      (1 to 100).foreach(_ => verifyDecoding(s"$ws[${ws}1$ws,${ws}2$ws,${ws}3$ws]$ws", list))
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

    "BigDecimal" - {
      roundTrip("1", BigDecimal(1))
      roundTrip("1000", BigDecimal(1000))
      roundTrip("12345.6789", BigDecimal(12345.6789))
      roundTrip("12345678901234567890.123456789", BigDecimal("12345678901234567890.123456789"))
    }

    "Byte Arrays" - {
      roundTrip("\"ASNFZ4mrze8SNFY=\"", hex"0123456789ABCDEF123456")
      roundTrip("""["","3q2+7w=="]""", List(Array.emptyByteArray, hex"DEADBEEF"))
    }

    "Error Position" - {
      intercept[Borer.Error.InvalidInputData[_ <: AnyRef]](decode[List[Int]]("[12,,42]")).getMessage ==>
      "Expected JSON value but got ',' (input position 4)"
    }
  }
}
