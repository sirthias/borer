/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.site

import io.bullet.borer.internal.Util.*
import io.bullet.borer.BorerSuite

class CustomCodecsSpec extends BorerSuite {

  test("From Unapply/Apply") {

    // #from-unapply-apply
    import io.bullet.borer.{Codec, Decoder, Encoder}

    case class Color(name: String, value: Int)

    given Encoder[Color] = Encoder.forProduct[Color]
    given Decoder[Color] = Decoder.forProduct[Color]

    // alternative: provide an Encoder and Decoder at the same time
    given Codec[Color] = Codec(
      Encoder.forProduct[Color],
      Decoder.forProduct[Color]
    )
    // #from-unapply-apply

    import io.bullet.borer.Json
    val color = Color("red", 0xFF0000)
    val json  = Json.encode(color).toUtf8String
    json ==> """["red",16711680]"""
    Json.decode(json.getBytes("UTF8")).to[Color].value ==> color

    // the alternative `Codec` must produce the same encoding
    val codec = summon[Codec[Color]]
    Json.encode(color)(using codec.encoder).toUtf8String ==> json
    Json.decode(json.getBytes("UTF8")).to[Color](using codec.decoder).value ==> color
  }

  test("Map/Contramap") {

    // #map-contramap
    import io.bullet.borer.{Decoder, Encoder}

    class Person(val name: String)

    // have `Person` be encoded as a simple CBOR/JSON text data item
    given Encoder[Person] = Encoder.forString.contramap[Person](_.name)
    given Decoder[Person] = Decoder.forString.map(new Person(_))
    // #map-contramap

    import io.bullet.borer.Json
    val person = new Person("Fred")
    val json   = Json.encode(person).toUtf8String
    json ==> """"Fred""""
    Json.decode(json.getBytes("UTF8")).to[Person].value.name ==> person.name
  }

  test("Manual") {

    // #manual
    import io.bullet.borer.{Decoder, Encoder}

    class Person(val name: String, val age: Int)

    given Encoder[Person] = Encoder { (writer, person) =>
      writer
        .writeArrayOpen(2)
        .writeString(person.name)
        .writeInt(person.age)
        .writeArrayClose()
    }

    given Decoder[Person] = Decoder { reader =>
      val unbounded = reader.readArrayOpen(2)
      val person    = new Person(
        reader.readString(),
        reader.readInt()
      )
      reader.readArrayClose(unbounded, person)
    }
    // #manual

    import io.bullet.borer.Json
    val person = new Person("Fred", 49)
    val json   = Json.encode(person).toUtf8String
    json ==> """["Fred",49]"""
    val p = Json.decode(json.getBytes("UTF8")).to[Person].value
    p.name ==> person.name
    p.age ==> person.age
  }

  test("Lookahead") {
    // #lookahead
    import io.bullet.borer.Decoder

    given Decoder[Either[String, Int]] =
      Decoder { reader =>
        if (reader.hasString) Left(reader.readString())
        else if (reader.hasInt) Right(reader.readInt())
        else reader.unexpectedDataItem(expected = "`String` or `Int`")
      }
    // #lookahead

    import io.bullet.borer.Json
    Json.decode("\"yeah\"".getBytes("UTF8")).to[Either[String, Int]].value ==> Left("yeah")
    Json.decode("42".getBytes("UTF8")).to[Either[String, Int]].value ==> Right(42)
    Json
      .decode("[]".getBytes("UTF8"))
      .to[Either[String, Int]]
      .valueTry
      .toString
      .contains("Expected `String` or `Int`") ==> true
  }

  test("Raw bytes") {
    // #raw-bytes
    import io.bullet.borer.{Cbor, Decoder, Encoder}

    // a value together with the exact bytes it was decoded from
    final case class KeepRaw[A](value: A, raw: Array[Byte])

    object KeepRaw {
      given [A: Decoder]: Decoder[KeepRaw[A]] =
        Decoder { reader =>
          val (value, raw) = reader.readWithRawBytes[A]()
          KeepRaw(value, raw)
        }

      // re-encodes the value with exactly its original bytes
      given [A]: Encoder[KeepRaw[A]] =
        Encoder((writer, x) => writer.writeValueAsRawBytes(x.raw))
    }

    // the CBOR encoding of `Vector(1, 2)` with the Ints using non-minimal encodings
    val bytes = hex"82" ++ hex"1801" ++ hex"190002"

    val decoded = Cbor.decode(bytes).to[Vector[KeepRaw[Int]]].value
    decoded.map(_.value) ==> Vector(1, 2)

    // a regular re-encoding would produce `hex"820102"`, but `KeepRaw` preserves the original bytes
    Cbor.encode(decoded).toByteArray ==> bytes
    // #raw-bytes
  }
}
