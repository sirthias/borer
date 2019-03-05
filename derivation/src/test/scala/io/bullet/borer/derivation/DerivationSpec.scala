/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.core.{Cbor, Dom}
import utest._

object DerivationSpec extends TestSuite {

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
    "Derivation" - {

      "simple array-based roundtrip" - {
        import ArrayBasedCodecs._

        implicit val colorEncoder = deriveEncoder[Color]
        implicit val colorDecoder = deriveDecoder[Color]
        implicit val fooEncoder   = deriveEncoder[Foo]
        implicit val fooDecoder   = deriveDecoder[Foo]

        val foo     = Foo()
        val encoded = Cbor.encode(foo).to[Array[Byte]].bytes

        Cbor.decode(encoded).to[Dom.Element].value ==> {
          import Dom.Element._
          Array(
            Value.Int(120),
            Value.Int(66),
            Value.Int(-10000),
            Value.Int(1234567),
            Value.Int(-1),
            Value.Float16(1.5f),
            Value.Double(26.8),
            Value.String("borer"),
            Array(
              Array(Value.Int(255), Value.Int(0), Value.Int(0), Value.Int(255)),
              Array(Value.Int(0), Value.Int(255), Value.Int(0), Value.Int(255)),
              Array(Value.Int(0), Value.Int(0), Value.Int(255), Value.Int(255))
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
          import Dom.Element._
          Map(
            "char"   → Value.Int(120),
            "byte"   → Value.Int(66),
            "short"  → Value.Int(-10000),
            "int"    → Value.Int(1234567),
            "long"   → Value.Int(-1),
            "float"  → Value.Float16(1.5f),
            "double" → Value.Double(26.8),
            "string" → Value.String("borer"),
            "colors" → Array(
              Map("red" → Value.Int(255), "green" → Value.Int(0), "blue"   → Value.Int(0), "alpha"   → Value.Int(255)),
              Map("red" → Value.Int(0), "green"   → Value.Int(255), "blue" → Value.Int(0), "alpha"   → Value.Int(255)),
              Map("red" → Value.Int(0), "green"   → Value.Int(0), "blue"   → Value.Int(255), "alpha" → Value.Int(255))
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
          import Dom.Element._
          Array(
            Array(Value.String("Dog"), Array(Value.Int(12), Value.String("Fred"))),
            Array(Value.String("TheCAT"), Array(Value.Float16(1.0f), Value.String("none"), Value.String("there"))),
            Array(Value.String("Dog"), Array(Value.Int(4), Value.String("Lolle"))),
            Array(Value.Int(42), Value.Bool(true)))
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
        val clazz = "io.bullet.borer.derivation.DerivationSpec.tests.Animal"
        error.getMessage ==> s"@TypeId collision: At least two subtypes of [$clazz] share the same TypeId [Dog]"
      }
    }
  }
}
