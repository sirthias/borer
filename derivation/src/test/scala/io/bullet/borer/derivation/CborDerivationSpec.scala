/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.{Cbor, Dom}
import utest._

object CborDerivationSpec extends TestSuite {
  import Dom._

  case class Color(red: Int = 0, green: Int = 0, blue: Int = 0, alpha: Int = 0xFF)

  case class Foo(char: Char = 'x',
                 byte: Byte = 0x42,
                 short: Short = -10000,
                 int: Int = 1234567,
                 long: Long = -1,
                 float: Float = 1.5f,
                 double: Double = 26.8,
                 string: String = "borer",
                 colors: List[Color] = List(Color(red = 0xFF), Color(green = 0xFF), Color(blue = 0xFF)))

  val tests = Tests {

    "simple array-based roundtrip" - {
      import ArrayBasedCodecs._

      implicit val colorEncoder = deriveEncoder[Color]
      implicit val colorDecoder = deriveDecoder[Color]
      implicit val fooEncoder   = deriveEncoder[Foo]
      implicit val fooDecoder   = deriveDecoder[Foo]

      val foo     = Foo()
      val encoded = Cbor.encode(foo).to[Array[Byte]].bytes

      Cbor.decode(encoded).to[Dom.Element].value ==> {
        ArrayElem.Sized(
          IntElem(120),
          IntElem(66),
          IntElem(-10000),
          IntElem(1234567),
          IntElem(-1),
          Float16Elem(1.5f),
          DoubleElem(26.8),
          StringElem("borer"),
          ArrayElem.Unsized(
            ArrayElem.Sized(IntElem(255), IntElem(0), IntElem(0), IntElem(255)),
            ArrayElem.Sized(IntElem(0), IntElem(255), IntElem(0), IntElem(255)),
            ArrayElem.Sized(IntElem(0), IntElem(0), IntElem(255), IntElem(255))
          ))
      }

      Cbor.decode(encoded).to[Foo].value ==> foo
    }

    "simple map-based roundtrip" - {
      import MapBasedCodecs._

      implicit val colorEncoder = deriveEncoder[Color]
      implicit val colorDecoder = deriveDecoder[Color]
      implicit val fooEncoder   = deriveEncoder[Foo]
      implicit val fooDecoder   = deriveDecoder[Foo]

      val foo     = Foo()
      val encoded = Cbor.encode(foo).to[Array[Byte]].bytes

      Cbor.decode(encoded).to[Dom.Element].value ==> {
        MapElem.Sized(
          "char"   → IntElem(120),
          "byte"   → IntElem(66),
          "short"  → IntElem(-10000),
          "int"    → IntElem(1234567),
          "long"   → IntElem(-1),
          "float"  → Float16Elem(1.5f),
          "double" → DoubleElem(26.8),
          "string" → StringElem("borer"),
          "colors" → ArrayElem.Unsized(
            MapElem.Sized("red" → IntElem(255), "green" → IntElem(0), "blue"   → IntElem(0), "alpha"   → IntElem(255)),
            MapElem.Sized("red" → IntElem(0), "green"   → IntElem(255), "blue" → IntElem(0), "alpha"   → IntElem(255)),
            MapElem.Sized("red" → IntElem(0), "green"   → IntElem(0), "blue"   → IntElem(255), "alpha" → IntElem(255))
          ))
      }

      Cbor.decode(encoded).to[Foo].value ==> foo
    }

    "ADT" - {
      import ArrayBasedCodecs._

      sealed trait Animal
      case class Dog(age: Int, name: String)                                        extends Animal
      @TypeId("TheCAT") case class Cat(weight: Double, color: String, home: String) extends Animal
      @TypeId(42) case class Mouse(tail: Boolean)                                   extends Animal

      implicit val animalEncoder = deriveEncoder[Animal]
      implicit val animalDecoder = deriveDecoder[Animal]

      val animals: List[Animal] = List(
        Dog(12, "Fred"),
        Cat(weight = 1.0, color = "none", home = "there"),
        Dog(4, "Lolle"),
        Mouse(true)
      )

      val encoded = Cbor.encode(animals).to[Array[Byte]].bytes

      Cbor.decode(encoded).to[Dom.Element].value ==> {
        ArrayElem.Unsized(
          ArrayElem.Sized(StringElem("Dog"), ArrayElem.Sized(IntElem(12), StringElem("Fred"))),
          ArrayElem
            .Sized(StringElem("TheCAT"), ArrayElem.Sized(Float16Elem(1.0f), StringElem("none"), StringElem("there"))),
          ArrayElem.Sized(StringElem("Dog"), ArrayElem.Sized(IntElem(4), StringElem("Lolle"))),
          ArrayElem.Sized(IntElem(42), BoolElem.True))
      }

      Cbor.decode(encoded).to[List[Animal]].value ==> animals
    }

    "ADT TypeId Collision" - {
      import ArrayBasedCodecs._

      sealed trait Animal
      case class Dog(age: Int, name: String)                                     extends Animal
      @TypeId("Dog") case class Cat(weight: Double, color: String, home: String) extends Animal
      @TypeId(42) case class Mouse(tail: Boolean)                                extends Animal

      val error = intercept[RuntimeException] {
        deriveEncoder[Animal]
      }
      val clazz = "io.bullet.borer.derivation.CborDerivationSpec.tests.Animal"
      error.getMessage ==> s"@TypeId collision: At least two subtypes of [$clazz] share the same TypeId [Dog]"
    }
  }
}
