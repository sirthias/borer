/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import io.bullet.borer.internal.Util.*

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8

class RawBytesSpec extends BorerSuite with TestUtils:

  // a value together with the raw bytes it was decoded from, re-encoded with exactly these bytes
  final case class KeepRaw[A](value: A, raw: Array[Byte]):
    def rawHex: String  = toHexString(raw)
    def rawUtf8: String = new String(raw, UTF_8)

  object KeepRaw:
    given [A: Decoder]: Decoder[KeepRaw[A]] = Decoder { r =>
      val (value, raw) = r.readWithRawBytes[A]()
      KeepRaw(value, raw)
    }
    given [A]: Encoder[KeepRaw[A]] = Encoder((w, x) => w.writeValueAsRawBytes(x.raw))

  case class Foo(int: Int, string: String)
  given Codec[Foo] = Codec.forProduct[Foo]

  def json(s: String): Array[Byte] = s.getBytes(UTF_8)

  //////////////////////////////// CBOR ////////////////////////////////

  test("CBOR: readWithRawBytes + writeValueAsRawBytes round trip with non-canonical encodings") {
    // [1 (non-minimal encoding), Foo(2, "a") (with non-minimal Int and array header), 3]
    val bytes   = hex"83" ++ hex"1801" ++ hex"9900021900026161" ++ hex"03"
    val decoded = Cbor.decode(bytes).to[(KeepRaw[Int], KeepRaw[Foo], KeepRaw[Int])].value

    decoded._1.value ==> 1
    decoded._1.rawHex ==> "1801"
    decoded._2.value ==> Foo(2, "a")
    decoded._2.rawHex ==> "9900021900026161"
    decoded._3.value ==> 3
    decoded._3.rawHex ==> "03"

    Cbor.encode(decoded).toByteArray ==> bytes
  }

  test("CBOR: readWithRawBytes in sequences and as last element of the input") {
    val bytes   = hex"9f" ++ hex"1801" ++ hex"1802" ++ hex"ff"
    val decoded = Cbor.decode(bytes).to[List[KeepRaw[Int]]].value
    decoded.map(_.value) ==> List(1, 2)
    decoded.map(_.rawHex) ==> List("1801", "1802")
    Cbor.encode(decoded).toByteArray ==> bytes

    val single = Cbor.decode(hex"1901f4").to[KeepRaw[Int]].value
    single.value ==> 500
    single.rawHex ==> "1901f4"
  }

  test("CBOR: readWithRawBytes with decoder lookahead") {
    // a decoder that peeks at the next data item after having read its value
    val peekingDecoder: Decoder[Int] = Decoder { r =>
      val x = r.readInt()
      r.hasString // triggers pulling the next data item
      x
    }
    val decoder = Decoder { r =>
      r.readArrayHeader(2)
      val (x, raw) = r.readWithRawBytes[Int]()(using peekingDecoder)
      val s        = r.readString()
      (x, toHexString(raw), s)
    }
    Cbor.decode(hex"82" ++ hex"1801" ++ hex"6161").to(using decoder).value ==> ((1, "1801", "a"))
  }

  test("CBOR: readElementAsRawBytes with complex and tagged elements") {
    val elem1  = hex"9fc11a514b67b0bf616101616282d901001801c063616263ffff" // unsized array with tags and map
    val elem2  = hex"a2616181c11a514b67b0616280"                           // sized map with tagged array element
    val elem3  = hex"d8185f4101420203ff"                                   // tagged unsized byte string
    val bytes  = hex"83" ++ elem1 ++ elem2 ++ elem3
    val result = Cbor
      .decode(bytes)
      .to(using Decoder { r =>
          r.readArrayHeader(3)
          Vector(r.readElementAsRawBytes(), r.readElementAsRawBytes(), r.readElementAsRawBytes())
        })
      .value
    result.map(toHexString) ==> Vector(elem1, elem2, elem3).map(toHexString)
  }

  test("CBOR: skipElement skips tagged values completely") {
    val decoder = Decoder { r =>
      r.readArrayHeader(3)
      r.skipElement() // [1(1363896240)]
      r.skipElement() // 1(1363896240)
      r.readInt()
    }
    Cbor.decode(hex"8381c11a514b67b0c11a514b67b0182a").to(using decoder).value ==> 42
  }

  test("CBOR: low-level rawBytesOfLastValue") {
    val decoder = Decoder { r =>
      r.readArrayHeader(2)
      r.dataItem()
      val start = r.cursor
      val foo   = r.read[Foo]()
      val raw   = r.rawBytesOfLastValue(start)
      r.readInt()
      foo -> toHexString(raw)
    }
    Cbor.decode(hex"82" ++ hex"82182a63666f6f" ++ hex"01").to(using decoder).value ==> (Foo(
      42,
      "foo") -> "82182a63666f6f")
  }

  test("CBOR: reading raw bytes from a ByteBuffer") {
    val buffer = ByteBuffer.wrap(hex"8218011802")
    Cbor.decode(buffer).to[Vector[KeepRaw[Int]]].value.map(_.rawHex) ==> Vector("1801", "1802")
  }

  test("CBOR: reading raw bytes from a non-random-access input fails") {
    val input = Iterator(hex"8218", hex"011802")
    intercept[Borer.Error.Unsupported[?]] {
      Cbor.decode(input).to[Vector[KeepRaw[Int]]].value
    }.getMessage ==> "Input `FromIterator` does not support random access to already consumed bytes (input position 1)"
  }

  test("CBOR: raw values count as exactly one element during validation") {
    Cbor
      .encode(Writer.Script(_.writeArrayHeader(2).writeValueAsRawBytes(hex"82182a63666f6f").writeInt(1)))
      .toByteArray ==> hex"8282182a63666f6f01"

    Cbor
      .encode(Writer.Script(_.writeMapHeader(1).writeValueAsRawBytes(hex"6161").writeValueAsRawBytes(hex"f5")))
      .toByteArray ==> hex"a16161f5"

    intercept[Borer.Error.UnexpectedEndOfInput[?]] {
      Cbor.encode(Writer.Script(_.writeArrayHeader(2).writeValueAsRawBytes(hex"01"))).toByteArray
    }

    intercept[Borer.Error.InvalidInputData[?]] { // BREAK after map key
      Cbor.encode(Writer.Script(_.writeMapStart().writeValueAsRawBytes(hex"6161").writeBreak())).toByteArray
    }

    Cbor.encode(Writer.Script(_.writeTag(Tag.EpochDateTime).writeValueAsRawBytes(hex"01"))).toByteArray ==> hex"c101"

    intercept[Borer.Error.InvalidInputData[?]] {
      Cbor.encode(Writer.Script(_.writeValueAsRawBytes(Array.emptyByteArray))).toByteArray
    }.getMessage ==> "Raw value bytes must not be empty (Output.ToByteArray index 0)"
  }

  test("CBOR: raw values in buffered encodings (ConcatEncoder)") {
    val a                                         = Writer.Script(_.writeArrayHeader(1).writeValueAsRawBytes(hex"1801"))
    val b                                         = Writer.Script(_.writeArrayHeader(1).writeValueAsRawBytes(hex"1802"))
    given Encoder[(Writer.Script, Writer.Script)] =
      summon[Encoder[Writer.Script]]
        .contramap[(Writer.Script, Writer.Script)](_._1)
        .concat(summon[Encoder[Writer.Script]].contramap[(Writer.Script, Writer.Script)](_._2))
    Cbor.encode(a -> b).toByteArray ==> hex"8218011802"
  }

  test("CBOR: logging of raw values") {
    val log = new java.lang.StringBuilder
    Cbor
      .encode(Writer.Script(_.writeArrayHeader(2).writeValueAsRawBytes(hex"1801").writeInt(2)))
      .withStringLogging(log, lineSep = "\n")
      .toByteArray ==> hex"82180102"
    log.toString ==>
    """    1| [
      |  1/2|     RAW[18 01]
      |  2/2|     2
      |    1| ]
      |    2| END
      |""".stripMargin
  }

  test("CBOR: transcoding of raw values") {
    val value = Writer.Script(_.writeArrayHeader(2).writeValueAsRawBytes(hex"9f1801c118029f80ffff").writeInt(3))
    Cbor.transEncode(value).transDecode.to[Dom.Element].value ==> Dom.ArrayElem.Sized(
      Dom.ArrayElem.Unsized(
        Dom.IntElem(1),
        Dom.TaggedElem(Tag.EpochDateTime, Dom.IntElem(2)),
        Dom.ArrayElem.Unsized(Dom.ArrayElem.Sized())),
      Dom.IntElem(3))

    intercept[Borer.Error.InvalidInputData[?]] { // more than one element
      Cbor.transEncode(Writer.Script(_.writeValueAsRawBytes(hex"0102"))).transDecode.to[Dom.Element].value
    }
  }

  //////////////////////////////// JSON ////////////////////////////////

  test("JSON: readWithRawBytes + writeValueAsRawBytes round trip") {
    val decoded =
      Json
        .decode(json("""[ 1.50 , {"a" : [true,  null] }  ,"x\u0041",[]]"""))
        .to[Vector[KeepRaw[Dom.Element]]]
        .value
    decoded.map(_.rawUtf8) ==> Vector("1.50", """{"a" : [true,  null] }""", """"x\u0041"""", "[]")
    new String(Json.encode(decoded).toByteArray, UTF_8) ==> """[1.50,{"a" : [true,  null] },"x\u0041",[]]"""
  }

  test("JSON: raw map keys and values") {
    val decoded =
      Json.decode(json("""{ "a" : 1.50 , "b":2 }""")).to[Map[KeepRaw[String], KeepRaw[Double]]].value.toList
    decoded.map { case (k, v) => k.rawUtf8 -> v.rawUtf8 } ==> List("\"a\"" -> "1.50", "\"b\"" -> "2")
    new String(Json.encode(decoded.toMap).toByteArray, UTF_8) ==> """{"a":1.50,"b":2}"""
  }

  test("JSON: raw top-level values") {
    Json.decode(json("  12.30  ")).to[KeepRaw[Double]].value.rawUtf8 ==> "12.30"
    Json.decode(json("12")).to[KeepRaw[Int]].value.rawUtf8 ==> "12"
    Json.decode(json("""{"a":[1,2]}""")).to[KeepRaw[Dom.Element]].value.rawUtf8 ==> """{"a":[1,2]}"""
  }

  test("JSON: readElementAsRawBytes") {
    val decoder = Decoder { r =>
      r.readArrayStart()
      val a = r.readElementAsRawBytes()
      val b = r.readElementAsRawBytes()
      r.readBreak()
      new String(a, UTF_8) -> new String(b, UTF_8)
    }
    Json.decode(json("""[{"x":[1, {}]},  "b"  ]""")).to(using decoder).value ==> ("""{"x":[1, {}]}""" -> "\"b\"")
  }

  test("JSON: pretty printing with raw values") {
    val value = Writer.Script(
      _.writeMapStart()
        .writeString("a")
        .writeValueAsRawBytes(json("[1,2]"))
        .writeString("b")
        .writeInt(3)
        .writeBreak())
    new String(Json.encode(value).withConfig(Json.EncodingConfig(indent = 2)).toByteArray, UTF_8) ==>
    """{
      |  "a": [1,2],
      |  "b": 3
      |}""".stripMargin
  }

  test("JSON: transcoding of raw values") {
    val value = Writer.Script(_.writeArrayStart().writeValueAsRawBytes(json("""{"a": [1, true]}""")).writeBreak())
    Json.transEncode(value).transDecode.to[Dom.Element].value ==> Dom.ArrayElem.Unsized(
      Dom.MapElem.Unsized("a" -> Dom.ArrayElem.Unsized(Dom.IntElem(1), Dom.BooleanElem(true))))
  }
