/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer._
import utest._

import scala.util.Failure

abstract class DerivationSpec(target: Target) extends TestSuite {
  import Dom._

  case class Empty()

  case class Color(red: Int = 0, green: Int = 0, blue: Int = 0, alpha: Int = 0xFF)

  case class Foo(char: Char = 'x',
                 byte: Byte = 0x42,
                 short: Short = -10000,
                 int: Int = 1234567,
                 long: Long,
                 float: Float = 1.5f,
                 double: Double = 26.8,
                 string: String = "borer",
                 empty: Empty = Empty(),
                 colors: List[Color] = List(Color(red = 0xFF), Color(green = 0xFF), Color(blue = 0xFF)))

  val foo = Foo(long = -1)

  def arrayBasedFooDom: ArrayElem
  def arrayBasedMissingElemErrorMsg: String

  def mapBasedFooDom: MapElem

  def animalsDom: Element

  val tests = Tests {

    "array-based" - {
      import ArrayBasedCodecs._
      implicit val emptyCodec = Codec(deriveEncoder[Empty], deriveDecoder[Empty])
      implicit val colorCodec = Codec(deriveEncoder[Color], deriveDecoder[Color])
      implicit val fooCodec   = Codec(deriveEncoder[Foo], deriveDecoder[Foo])

      "size match" - {
        val encoded = target.encode(foo).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> arrayBasedFooDom
        target.decode(encoded).to[Foo].value ==> foo
      }

      "decoding w/ missing elements" - {
        val dom     = transform(arrayBasedFooDom)(_.tail)
        val encoded = target.encode(dom).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> dom
        assertMatch(target.decode(encoded).to[Foo].valueTry) {
          case Failure(e) if e.getMessage == arrayBasedMissingElemErrorMsg ⇒ // ok
        }
      }
    }

    "map-based" - {
      import MapBasedCodecs._
      implicit val emptyCodec = Codec(deriveEncoder[Empty], deriveDecoder[Empty])
      implicit val colorCodec = Codec(deriveEncoder[Color], deriveDecoder[Color])
      implicit val fooCodec   = Codec(deriveEncoder[Foo], deriveDecoder[Foo])

      "size match" - {
        val encoded = target.encode(foo).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> mapBasedFooDom
        target.decode(encoded).to[Foo].value ==> foo
      }

      "size match unordered" - {
        val dom     = transform(mapBasedFooDom)(_.toMap.toList)
        val encoded = target.encode(dom).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> dom
        target.decode(encoded).to[Foo].value ==> foo
      }

      "missing members w/ default value" - {
        val dom     = transform(mapBasedFooDom)(_.filter(_._1 != StringElem("int")))
        val encoded = target.encode(dom).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> dom
        target.decode(encoded).to[Foo].value ==> foo
      }

      "missing members w/o default value" - {
        val dom     = transform(mapBasedFooDom)(_.filter(_._1 != StringElem("long")))
        val encoded = target.encode(dom).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> dom

        val errorMsg =
          "Missing map key [long] for decoding an instance of type [io.bullet.borer.derivation.DerivationSpec.Foo]"
        assertMatch(target.decode(encoded).to[Foo].valueTry) {
          case Failure(e) if e.getMessage startsWith errorMsg ⇒ // ok
        }
      }

      "duplicate members before fill completion" - {
        val dom     = transform(mapBasedFooDom)(x ⇒ x.take(3) ::: x.drop(2))
        val encoded = target.encode(dom).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> dom

        assertMatch(target.decode(encoded).to[Foo].valueTry) {
          case Failure(e) if e.getMessage startsWith "Duplicate map key [short] encountered" ⇒ // ok
        }
      }

      "duplicate members after fill completion" - {
        val dom     = transform(mapBasedFooDom)(x ⇒ x :+ x(3))
        val encoded = target.encode(dom).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> dom

        assertMatch(target.decode(encoded).to[Foo].valueTry) {
          case Failure(e) if e.getMessage startsWith "Duplicate map key [int] encountered" ⇒ // ok
        }
      }

      "surplus members before fill completion" - {
        val extraMember = StringElem("yeah") → StringElem("xxx")
        val dom         = transform(mapBasedFooDom)(x ⇒ (x.take(3) :+ extraMember) ++ x.drop(3))
        val encoded     = target.encode(dom).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> dom
        target.decode(encoded).to[Foo].value ==> foo
      }

      "surplus members after fill completion" - {
        val extraMember = StringElem("yeah") → StringElem("xxx")
        val dom         = transform(mapBasedFooDom)(_ :+ extraMember)
        val encoded     = target.encode(dom).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> dom
        target.decode(encoded).to[Foo].value ==> foo
      }

      "special option support" - {
        case class Qux0(int: Int)
        case class Qux(int: Int, optDouble: Option[Double] = None)

        implicit val qux0Codec = Codec(deriveEncoder[Qux0], deriveDecoder[Qux0])
        implicit val quxCodec  = Codec(deriveEncoder[Qux], deriveDecoder[Qux])

        val qux0        = Qux0(42)
        val quxWithNone = Qux(42)
        val quxWithSome = Qux(42, Some(3.45))

        val quxWithSomeEncoded = target.encode(quxWithSome).to[Array[Byte]].bytes
        target.decode(quxWithSomeEncoded).to[Qux].value ==> quxWithSome

        val qux0Encoded        = target.encode(qux0).to[Array[Byte]].bytes
        val quxWithNoneEncoded = target.encode(quxWithNone).to[Array[Byte]].bytes

        toHexString(qux0Encoded) ==> toHexString(quxWithNoneEncoded)
        target.decode(quxWithNoneEncoded).to[Qux].value ==> quxWithNone
      }
    }

    "ADT" - {
      import ArrayBasedCodecs._

      sealed trait Animal
      case class Dog(age: Int, name: String)                                        extends Animal
      @TypeId("TheCAT") case class Cat(weight: Double, color: String, home: String) extends Animal
      @TypeId(42) case class Mouse(tail: Boolean)                                   extends Animal

      implicit val animalCodec = Codec(deriveEncoder[Animal], deriveDecoder[Animal])

      val animals: List[Animal] = List(
        Dog(12, "Fred"),
        Cat(weight = 1.0, color = "none", home = "there"),
        Dog(4, "Lolle"),
        Mouse(true)
      )

      val encoded = target.encode(animals).to[Array[Byte]].bytes
      target.decode(encoded).to[Element].value ==> animalsDom
      target.decode(encoded).to[List[Animal]].value ==> animals
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
      val clazz = s"io.bullet.borer.derivation.DerivationSpec.tests.Animal"
      error.getMessage ==> s"@TypeId collision: At least two subtypes of [$clazz] share the same TypeId [Dog]"
    }
  }

  def transform(elem: ArrayElem)(f: Vector[Element] ⇒ Vector[Element]): ArrayElem =
    elem match {
      case ArrayElem.Sized(x)   ⇒ ArrayElem.Sized(f(x))
      case ArrayElem.Unsized(x) ⇒ ArrayElem.Unsized(f(x))
    }

  def transform(elem: MapElem)(f: List[(Element, Element)] ⇒ List[(Element, Element)]): MapElem =
    elem match {
      case MapElem.Sized(_, keys, values)   ⇒ MapElem.Sized(f((keys zip values).toList): _*)
      case MapElem.Unsized(_, keys, values) ⇒ MapElem.Unsized(f((keys zip values).toList): _*)
    }

  final def toHexString(bytes: Array[Byte]): String = bytes.map(x ⇒ f"${x & 0xFF}%02x").mkString
}
