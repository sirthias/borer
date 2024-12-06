/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.site

import io.bullet.borer.internal.Util.*
import io.bullet.borer.BorerSuite

class JsonSpecificsSpec extends BorerSuite {

  test("writeEmptyArray") {
    // #writeEmptyArray
    import io.bullet.borer.Writer

    def writeEmptyArray(w: Writer): w.type =
      if (w.writingJson) w.writeArrayStart().writeBreak()
      else w.writeArrayHeader(0) // fixed-sized Arrays are not supported in JSON
    // #writeEmptyArray

    import io.bullet.borer.*

    object Foo

    given Encoder[Foo.type] = Encoder((w, _) => writeEmptyArray(w))

    Cbor.encode(Foo).toByteArray ==> hex"80"
    Json.encode(Foo).toUtf8String ==> "[]"
  }

  test("writeArrayOpen-close") {
    // #writeArrayOpen-close
    import io.bullet.borer.Writer

    def writeAsUnaryArray(w: Writer, s: String): w.type =
      w.writeArrayOpen(1) // automatically chooses the most efficient
        .writeString(s)
        .writeArrayClose() // way to write an array of size one
    // #writeArrayOpen-close

    import io.bullet.borer.*

    object Foo

    given Encoder[Foo.type] = Encoder((w, _) => writeAsUnaryArray(w, "foo"))

    Cbor.encode(Foo).toByteArray ==> hex"8163666F6F"
    Json.encode(Foo).toUtf8String ==> """["foo"]"""
  }

  test("alternative base encoding") {
    // #alternative-base-encoding
    import io.bullet.borer.{Decoder, Encoder, Json}
    import io.bullet.borer.encodings.BaseEncoding

    val binaryData = hex"DEADBEEF"

    // Json.encode(binaryData).toByteArray or
    Json.encode(binaryData).toUtf8String ==> """"3q2+7w==""""

    {
      // we need to explicitly define the encoder as well as the decoder
      // in order to "override" the defaults for Array[Byte] on either side
      given Encoder[Array[Byte]] = Encoder.forByteArray(BaseEncoding.zbase32)
      given Decoder[Array[Byte]] = Decoder.forByteArray(BaseEncoding.zbase32)

      Json.encode(binaryData).toUtf8String ==> """"54s575a""""
    }
    // #alternative-base-encoding
  }

  test("json pretty printing") {
    // #json-pretty-printing
    import io.bullet.borer.{Codec, Json}
    import io.bullet.borer.derivation.MapBasedCodecs.*

    sealed trait Animal derives Codec.All
    case class Dog(age: Int, name: String)                      extends Animal
    case class Cat(weight: Double, color: String, home: String) extends Animal
    case class Mouse(colorRGB: List[Int], tail: Boolean)        extends Animal

    val animals = List(
      Dog(5, "Rufus"),
      Cat(4.7, "red", "next door"),
      Mouse(colorRGB = List(73, 42, 64), tail = false),
    )

    Json
      .encode(animals)
      .withPrettyRendering(indent = 2) // enables pretty printing
      .toUtf8String ==>
    """|[
       |  {
       |    "Dog": {
       |      "age": 5,
       |      "name": "Rufus"
       |    }
       |  },
       |  {
       |    "Cat": {
       |      "weight": 4.7,
       |      "color": "red",
       |      "home": "next door"
       |    }
       |  },
       |  {
       |    "Mouse": {
       |      "colorRGB": [
       |        73,
       |        42,
       |        64
       |      ],
       |      "tail": false
       |    }
       |  }
       |]""".stripMargin
    // #json-pretty-printing
  }
}
