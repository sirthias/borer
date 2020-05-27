package io.bullet.borer.site

import utest._

object CustomCodecsSpec extends TestSuite {

  val tests = Tests {

    "From Unapply/Apply" - {

      //#from-unapply-apply
      import io.bullet.borer.{Codec, Decoder, Encoder}

      case class Color(name: String, value: Int)

      implicit val encoder = Encoder.from(Color.unapply _)
      implicit val decoder = Decoder.from(Color.apply _)

      // alternative: provide an Encoder and Decoder at the same time
      implicit val codec = Codec(
        Encoder.from(Color.unapply _),
        Decoder.from(Color.apply _)
      )
      //#from-unapply-apply

      import io.bullet.borer.Json
      val color = Color("red", 0xFF0000)
      val json  = Json.encode(color).toUtf8String
      json ==> """["red",16711680]"""
      Json.decode(json getBytes "UTF8").to[Color].value ==> color
    }

    "Map/Contramap" - {

      //#map-contramap
      import io.bullet.borer.{Decoder, Encoder}

      class Person(val name: String)

      // have `Person` be encoded as a simple CBOR/JSON text data item
      implicit val personEncoder = Encoder.forString.contramap[Person](_.name)
      implicit val personDecoder = Decoder.forString.map(new Person(_))
      //#map-contramap

      import io.bullet.borer.Json
      val person = new Person("Fred")
      val json   = Json.encode(person).toUtf8String
      json ==> """"Fred""""
      Json.decode(json getBytes "UTF8").to[Person].value.name ==> person.name
    }

    "Manual" - {

      //#manual
      import io.bullet.borer.{Decoder, Encoder}

      class Person(val name: String, val age: Int)

      implicit val encoder: Encoder[Person] = Encoder { (writer, person) =>
        writer
          .writeArrayOpen(2)
          .writeString(person.name)
          .writeInt(person.age)
          .writeArrayClose()
      }

      implicit val decoder: Decoder[Person] = Decoder { reader =>
        val unbounded = reader.readArrayOpen(2)
        val person = new Person(
          reader.readString(),
          reader.readInt()
        )
        reader.readArrayClose(unbounded, person)
      }
      //#manual

      import io.bullet.borer.Json
      val person = new Person("Fred", 49)
      val json   = Json.encode(person).toUtf8String
      json ==> """["Fred",49]"""
      val p = Json.decode(json getBytes "UTF8").to[Person].value
      p.name ==> person.name
      p.age ==> person.age
    }

    "Lookahead" - {
      //#lookahead
      import io.bullet.borer.Decoder

      implicit val eitherStringIntDecoder: Decoder[Either[String, Int]] =
        Decoder { reader =>
          if (reader.hasString) Left(reader.readString())
          else if (reader.hasInt) Right(reader.readInt())
          else reader.unexpectedDataItem(expected = "`String` or `Int`")
        }
      //#lookahead

      import io.bullet.borer.Json
      Json.decode("\"yeah\"" getBytes "UTF8").to[Either[String, Int]].value ==> Left("yeah")
      Json.decode("42" getBytes "UTF8").to[Either[String, Int]].value ==> Right(42)
      Json
        .decode("[]" getBytes "UTF8")
        .to[Either[String, Int]]
        .valueTry
        .toString
        .contains("Expected `String` or `Int`") ==> true
    }
  }
}
