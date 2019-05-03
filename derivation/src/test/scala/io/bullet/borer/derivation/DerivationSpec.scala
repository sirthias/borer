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

  case class Foo(
      char: Char = 'x',
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

  case class Hundred(
      x00: Boolean = false,
      x01: Char = 'x',
      x02: Byte = 0x42,
      x03: Short = -10000,
      x04: Int = 1234567,
      x05: Long = Int.MaxValue.toLong + 1,
      x06: Float = 1.5f,
      x07: Double = 26.8,
      x08: String = "borer",
      x09: Color = Color(),
      x10: Boolean = false,
      x11: Char = 'x',
      x12: Byte = 0x42,
      x13: Short = -10000,
      x14: Int = 1234567,
      x15: Long = Int.MaxValue.toLong + 1,
      x16: Float = 1.5f,
      x17: Double = 26.8,
      x18: String = "borer",
      x19: Color = Color(),
      x20: Boolean = false,
      x21: Char = 'x',
      x22: Byte = 0x42,
      x23: Short = -10000,
      x24: Int = 1234567,
      x25: Long = Int.MaxValue.toLong + 1,
      x26: Float = 1.5f,
      x27: Double = 26.8,
      x28: String = "borer",
      x29: Color = Color(),
      x30: Boolean = false,
      x31: Char = 'x',
      x32: Byte = 0x42,
      x33: Short = -10000,
      x34: Int = 1234567,
      x35: Long = Int.MaxValue.toLong + 1,
      x36: Float = 1.5f,
      x37: Double = 26.8,
      x38: String = "borer",
      x39: Color = Color(),
      x40: Boolean = false,
      x41: Char = 'x',
      x42: Byte = 0x42,
      x43: Short = -10000,
      x44: Int = 1234567,
      x45: Long = Int.MaxValue.toLong + 1,
      x46: Float = 1.5f,
      x47: Double,
      x48: String = "borer",
      x49: Color = Color(),
      x50: Boolean = false,
      x51: Char = 'x',
      x52: Byte = 0x42,
      x53: Short = -10000,
      x54: Int = 1234567,
      x55: Long = Int.MaxValue.toLong + 1,
      x56: Float = 1.5f,
      x57: Double = 26.8,
      x58: String = "borer",
      x59: Color = Color(),
      x60: Boolean = false,
      x61: Char = 'x',
      x62: Byte = 0x42,
      x63: Short = -10000,
      x64: Int = 1234567,
      x65: Long = Int.MaxValue.toLong + 1,
      x66: Float = 1.5f,
      x67: Double = 26.8,
      x68: String = "borer",
      x69: Color = Color(),
      x70: Boolean = false,
      x71: Char = 'x',
      x72: Byte = 0x42,
      x73: Short = -10000,
      x74: Int = 1234567,
      x75: Long = Int.MaxValue.toLong + 1,
      x76: Float = 1.5f,
      x77: Double = 26.8,
      x78: String = "borer",
      x79: Color = Color(),
      x80: Boolean = false,
      x81: Char = 'x',
      x82: Byte = 0x42,
      x83: Short = -10000,
      x84: Int = 1234567,
      x85: Long = Int.MaxValue.toLong + 1,
      x86: Float = 1.5f,
      x87: Double = 26.8,
      x88: String = "borer",
      x89: Color = Color(),
      x90: Boolean = false,
      x91: Char,
      x92: Byte = 0x42,
      x93: Short = -10000,
      x94: Int = 1234567,
      x95: Long = Int.MaxValue.toLong + 1,
      x96: Float = 1.5f,
      x97: Double = 26.8,
      x98: String = "borer",
      x99: Color = Color(),
  )

  val hundred = Hundred(x47 = 26.8, x91 = 'x')

  def arrayBasedFooDom: ArrayElem
  def arrayBasedMissingElemErrorMsg: String

  def mapBasedFooDom: MapElem
  def mapBased100Dom: MapElem

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
      implicit val emptyCodec   = Codec(deriveEncoder[Empty], deriveDecoder[Empty])
      implicit val colorCodec   = Codec(deriveEncoder[Color], deriveDecoder[Color])
      implicit val fooCodec     = Codec(deriveEncoder[Foo], deriveDecoder[Foo])
      implicit val hundredCodec = Codec(deriveEncoder[Hundred], deriveDecoder[Hundred])

      def sizeMatch[T: Encoder: Decoder](value: T, dom: MapElem): Unit = {
        val encoded = target.encode(value).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> dom
        target.decode(encoded).to[T].value ==> value
      }

      def sizeMatchUnordered[T: Encoder: Decoder](value: T, dom: MapElem): Unit = {
        val unorderedDom = transform(dom)(_.toMap.toList)
        val encoded      = target.encode(unorderedDom).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> unorderedDom
        target.decode(encoded).to[T].value ==> value
      }

      def missingMembersWithDefaultValue[T: Encoder: Decoder](value: T, dom: MapElem): Unit = {
        val partialDom = transform(dom)(_.filter(_._1 != StringElem("int")))
        val encoded    = target.encode(partialDom).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> partialDom
        target.decode(encoded).to[T].value ==> value
      }

      def missingMembersWithoutDefaultValue[T: Encoder: Decoder](dom: MapElem, missingKey: String): Unit = {
        val partialDom = transform(dom)(_.filter(_._1 != StringElem(missingKey)))
        val encoded    = target.encode(partialDom).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> partialDom

        val errorMsg =
          s"Missing map key [$missingKey] for decoding an instance of type [io.bullet.borer.derivation.DerivationSpec."
        assertMatch(target.decode(encoded).to[T].valueTry) {
          case Failure(e) if e.getMessage startsWith errorMsg ⇒ // ok
        }
      }

      def dupMemberBeforeFillCompletion[T: Encoder: Decoder](dom: MapElem, memberIx: Int, key: String): Unit = {
        val badDom  = transform(dom)(x ⇒ x.take(memberIx) ::: x.drop(memberIx - 1))
        val encoded = target.encode(badDom).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> badDom

        assertMatch(target.decode(encoded).to[T].valueTry) {
          case Failure(e) if e.getMessage startsWith s"Duplicate map key [$key] encountered" ⇒ // ok
        }
      }

      def dupMemberAfterFillCompletion[T: Encoder: Decoder](dom: MapElem, memberIx: Int, key: String): Unit = {
        val badDom  = transform(dom)(x ⇒ x :+ x(memberIx))
        val encoded = target.encode(badDom).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> badDom

        assertMatch(target.decode(encoded).to[T].valueTry) {
          case Failure(e) if e.getMessage startsWith s"Duplicate map key [$key] encountered" ⇒ // ok
        }
      }

      def extraMemberBeforeFillCompletion[T: Encoder: Decoder](value: T, dom: MapElem, ix: Int): Unit = {
        val extraMember = StringElem("yeah") → StringElem("xxx")
        val fatDom      = transform(dom)(x ⇒ (x.take(ix) :+ extraMember) ++ x.drop(ix))
        val encoded     = target.encode(fatDom).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> fatDom
        target.decode(encoded).to[T].value ==> value
      }

      def extraMemberAfterFillCompletion[T: Encoder: Decoder](value: T, dom: MapElem): Unit = {
        val extraMember = StringElem("extra") → StringElem("xxx")
        val fatDom      = transform(dom)(_ :+ extraMember)
        val encoded     = target.encode(fatDom).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> fatDom
        target.decode(encoded).to[T].value ==> value
      }

      def complexExtraMemberBeforeFC[T: Encoder: Decoder](value: T, dom: MapElem, ix: Int): Unit = {
        val extraMember = StringElem("extra") → dom
        val fatDom      = transform(dom)(x ⇒ (x.take(ix) :+ extraMember) ++ x.drop(ix))
        val encoded     = target.encode(fatDom).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> fatDom
        target.decode(encoded).to[T].value ==> value
      }

      def complexExtraMemberAfterFC[T: Encoder: Decoder](value: T, dom: MapElem): Unit = {
        val extraMember = StringElem("extra") → dom
        val fatDom      = transform(dom)(_ :+ extraMember)
        val encoded     = target.encode(fatDom).to[Array[Byte]].bytes
        target.decode(encoded).to[Element].value ==> fatDom
        target.decode(encoded).to[T].value ==> value
      }

      "Foo" - {
        "size match" - sizeMatch(foo, mapBasedFooDom)
        "size match unordered" - sizeMatchUnordered(foo, mapBasedFooDom)
        "missing member w/ default value" - missingMembersWithDefaultValue(foo, mapBasedFooDom)
        "missing member w/o default value" - missingMembersWithoutDefaultValue[Foo](mapBasedFooDom, "long")
        "duplicate member before fill completion" - dupMemberBeforeFillCompletion[Foo](mapBasedFooDom, 3, "short")
        "duplicate member after fill completion" - dupMemberAfterFillCompletion[Foo](mapBasedFooDom, 5, "float")
        "extra member before fill completion" - extraMemberBeforeFillCompletion(foo, mapBasedFooDom, 3)
        "extra member after fill completion" - extraMemberAfterFillCompletion(foo, mapBasedFooDom)
        "complex extra member before fill completion" - complexExtraMemberBeforeFC(foo, mapBasedFooDom, 3)
        "complex extra member after fill completion" - complexExtraMemberAfterFC(foo, mapBasedFooDom)
      }

      "Hundred" - {
        "size match" - sizeMatch(hundred, mapBased100Dom)
        "size match unordered" - sizeMatchUnordered(hundred, mapBased100Dom)
        "missing member w/ default value" - missingMembersWithDefaultValue(hundred, mapBased100Dom)
        "missing lo member w/o default value" - missingMembersWithoutDefaultValue[Hundred](mapBased100Dom, "x47")
        "missing hi member w/o default value" - missingMembersWithoutDefaultValue[Hundred](mapBased100Dom, "x91")
        "duplicate lo member before fill completion" - dupMemberBeforeFillCompletion[Hundred](mapBased100Dom, 29, "x28")
        "duplicate hi member before fill completion" - dupMemberBeforeFillCompletion[Hundred](mapBased100Dom, 73, "x72")
        "extra lo member before fill completion" - extraMemberBeforeFillCompletion(hundred, mapBased100Dom, 13)
        "extra hi member before fill completion" - extraMemberBeforeFillCompletion(hundred, mapBased100Dom, 97)
        "extra member after fill completion" - extraMemberAfterFillCompletion(hundred, mapBased100Dom)
        "complex extra lo member before fill completion" - complexExtraMemberBeforeFC(hundred, mapBased100Dom, 61)
        "complex extra hi member before fill completion" - complexExtraMemberBeforeFC(hundred, mapBased100Dom, 65)
        "complex extra member after fill completion" - complexExtraMemberAfterFC(hundred, mapBased100Dom)
      }

      "Option with default value None" - {
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

      "List with default value Nil" - {
        case class Qux0(int: Int)
        case class Qux(int: Int, optList: List[Float] = Nil)

        implicit val qux0Codec = Codec(deriveEncoder[Qux0], deriveDecoder[Qux0])
        implicit val quxCodec  = Codec(deriveEncoder[Qux], deriveDecoder[Qux])

        val qux0       = Qux0(42)
        val quxWithNil = Qux(42)
        val quxWithNel = Qux(42, List(3.45f))

        val quxWithNelEncoded = target.encode(quxWithNel).to[Array[Byte]].bytes
        target.decode(quxWithNelEncoded).to[Qux].value ==> quxWithNel

        val qux0Encoded       = target.encode(qux0).to[Array[Byte]].bytes
        val quxWithNilEncoded = target.encode(quxWithNil).to[Array[Byte]].bytes

        toHexString(qux0Encoded) ==> toHexString(quxWithNilEncoded)
        target.decode(quxWithNilEncoded).to[Qux].value ==> quxWithNil
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
