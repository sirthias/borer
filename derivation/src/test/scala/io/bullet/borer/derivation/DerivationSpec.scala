/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer._
import utest._

import scala.util.{Failure, Try}
import scala.util.control.NonFatal

abstract class DerivationSpec(target: Target) extends AbstractBorerSpec {
  import Dom._

  def tryDecode[T: Decoder](encoded: String): Try[T]

  case class Empty()

  case class Color(red: Int = 0, green: Int = 0, blue: Int = 0, alpha: Int = 0xFF)

  case class Foo(
      char: Char = 'x',
      byte: Byte = 0x42,
      short: Short = -10000,
      int: Int = 1234567,
      long: Long,
      float: Float = 1.5f,
      @key("dub") double: Double = 26.8,
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
  def mapBasedFooDomNoDefaults: MapElem

  def mapBased100Dom: MapElem
  def mapBased100DomNoDefaults: MapElem

  def arrayBasedAnimalsDom: Element
  def mapBasedAnimalsDom: Element

  def arrayBasedCaseObjectAdtDom: Element
  def mapBasedCaseObjectAdtDom: Element

  def arrayBasedBarDom: Element
  def mapBasedBarDom: Element

  def recursiveBoxEncoded: String

  val tests = Tests {

    "array-based" - {
      import ArrayBasedCodecs._
      implicit val emptyCodec = deriveCodec[Empty]
      implicit val colorCodec = deriveCodec[Color]
      implicit val fooCodec   = deriveCodec[Foo]

      "size match" - {
        val encoded = encode(foo)
        decode[Element](encoded) ==> arrayBasedFooDom
        decode[Foo](encoded) ==> foo
      }

      "decoding w/ missing elements" - {
        val dom     = transform(arrayBasedFooDom)(_.tail)
        val encoded = encode(dom)
        decode[Element](encoded) ==> dom
        assertMatch(tryDecode[Foo](encoded)) {
          case Failure(e) if e.getMessage == arrayBasedMissingElemErrorMsg => // ok
        }
      }

      "ADT" - {
        import ADT._

        implicit val dogCodec    = deriveCodec[Dog]
        implicit val catCodec    = deriveCodec[Cat]
        implicit val mouseCodec  = deriveCodec[Mouse]
        implicit val animalCodec = deriveCodec[Animal]

        val animals: List[Animal] = List(
          Dog(12, "Fred"),
          Cat(weight = 1.0, color = "none", home = "there"),
          Dog(4, "Lolle"),
          Mouse(true)
        )

        val encoded = encode(animals)
        decode[Element](encoded) ==> arrayBasedAnimalsDom
        decode[List[Animal]](encoded) ==> animals
      }

      "ADT Key Collision" - {
        import AdtWithKeyCollision._

        implicit val dogCodec   = deriveCodec[Dog]
        implicit val catCodec   = deriveCodec[Cat]
        implicit val mouseCodec = deriveCodec[Mouse]

        Scalac
          .typecheck("deriveEncoder[Animal]")
          .assertErrorMsgMatches(
            "@key collision: sub types `io.bullet.borer.derivation.AdtWithKeyCollision.Cat` and " +
              "`io.bullet.borer.derivation.AdtWithKeyCollision.Dog` of ADT " +
              "`io.bullet.borer.derivation.AdtWithKeyCollision.Animal` share the same type id `Dog`"
          )
      }

      "Diamond" - {
        sealed trait A
        sealed trait B       extends A
        sealed trait C       extends A
        case class D(a: Int) extends B with C

        implicit val d = deriveCodec[D]
        implicit val a = deriveCodec[A]
      }

      "Case Objects" - {
        sealed trait CaseObjectAdt
        case class Err(reason: String) extends CaseObjectAdt
        case object Ok                 extends CaseObjectAdt

        implicit val errCodec = deriveCodec[Err]
        implicit val okCodec  = deriveCodec[Ok.type]
        implicit val adtCodec = deriveCodec[CaseObjectAdt]

        val values: List[CaseObjectAdt] = List(Err("foo"), Ok)
        val encoded                     = encode(values)
        decode[Element](encoded) ==> arrayBasedCaseObjectAdtDom
        decode[List[CaseObjectAdt]](encoded) ==> values
      }

      "Basic Type with custom Codec" - {
        case class Bar(i: Int, s: String)

        import Encoder.StringNumbers._
        import Decoder.StringNumbers._
        implicit val barCodec = deriveCodec[Bar]

        val bar     = Bar(42, "bar")
        val encoded = encode(bar)
        decode[Element](encoded) ==> arrayBasedBarDom
        decode[Bar](encoded) ==> bar
      }
    }

    "map-based" - {
      import MapBasedCodecs._
      implicit val emptyCodec   = deriveCodec[Empty]
      implicit val colorCodec   = deriveCodec[Color]
      implicit val fooCodec     = deriveCodec[Foo]
      implicit val hundredCodec = deriveCodec[Hundred]

      def sizeMatch[T: Encoder: Decoder](value: T, domNoDefaults: MapElem, dom: MapElem): Unit = {
        encode(value) ==> encode(domNoDefaults)
        decode[T](encode(dom)) ==> value
      }

      def sizeMatchUnordered[T: Encoder: Decoder](value: T, dom: MapElem): Unit = {
        val unorderedDom = transform(dom)(_.toMap.toList)
        val encoded      = encode(unorderedDom)
        decode[Element](encoded) ==> unorderedDom
        decode[T](encoded) ==> value
      }

      def missingMembersWithDefaultValue[T: Encoder: Decoder](value: T, dom: MapElem): Unit = {
        val partialDom = transform(dom)(_.filter(_._1 != StringElem("int")))
        val encoded    = encode(partialDom)
        decode[Element](encoded) ==> partialDom
        decode[T](encoded) ==> value
      }

      def missingMembersWithoutDefaultValue[T: Encoder: Decoder](dom: MapElem, missingKey: String): Unit = {
        val partialDom = transform(dom)(_.filter(_._1 != StringElem(missingKey)))
        val encoded    = encode(partialDom)
        decode[Element](encoded) ==> partialDom

        val errorMsg = s"""Cannot decode `.*` instance due to missing map key "$missingKey".*"""
        assertMatch(tryDecode[T](encoded)) {
          case Failure(e) if e.getMessage matches errorMsg => // ok
        }
      }

      def dupMemberBeforeFillCompletion[T: Encoder: Decoder](dom: MapElem, memberIx: Int, key: String): Unit = {
        val badDom  = transform(dom)(x => x.take(memberIx) ::: x.drop(memberIx - 1))
        val encoded = encode(badDom)
        decode[Element](encoded) ==> badDom

        assertMatch(tryDecode[T](encoded)) {
          case Failure(e) if e.getMessage startsWith s"Duplicate map key `$key` encountered" => // ok
        }
      }

      def dupMemberAfterFillCompletion[T: Encoder: Decoder](value: T, dom: MapElem, memberIx: Int): Unit = {
        val badDom  = transform(dom)(x => x :+ x(memberIx))
        val encoded = encode(badDom)
        decode[Element](encoded) ==> badDom
        decode[T](encoded)
      }

      def extraMemberBeforeFillCompletion[T: Encoder: Decoder](value: T, dom: MapElem, ix: Int): Unit = {
        val extraMember = StringElem("yeah") -> StringElem("xxx")
        val fatDom      = transform(dom)(x => (x.take(ix) :+ extraMember) ++ x.drop(ix))
        val encoded     = encode(fatDom)
        decode[Element](encoded) ==> fatDom
        decode[T](encoded) ==> value
      }

      def extraMemberAfterFillCompletion[T: Encoder: Decoder](value: T, dom: MapElem): Unit = {
        val extraMember = StringElem("extra") -> StringElem("xxx")
        val fatDom      = transform(dom)(_ :+ extraMember)
        val encoded     = encode(fatDom)
        decode[Element](encoded) ==> fatDom
        decode[T](encoded) ==> value
      }

      def complexExtraMemberBeforeFC[T: Encoder: Decoder](value: T, dom: MapElem, ix: Int): Unit = {
        val extraMember = StringElem("extra") -> dom
        val fatDom      = transform(dom)(x => (x.take(ix) :+ extraMember) ++ x.drop(ix))
        val encoded     = encode(fatDom)
        decode[Element](encoded) ==> fatDom
        decode[T](encoded) ==> value
      }

      def complexExtraMemberAfterFC[T: Encoder: Decoder](value: T, dom: MapElem): Unit = {
        val extraMember = StringElem("extra") -> dom
        val fatDom      = transform(dom)(_ :+ extraMember)
        val encoded     = encode(fatDom)
        decode[Element](encoded) ==> fatDom
        decode[T](encoded) ==> value
      }

      "Foo" - {
        "size match" - sizeMatch(foo, mapBasedFooDomNoDefaults, mapBasedFooDom)
        "size match unordered" - sizeMatchUnordered(foo, mapBasedFooDom)
        "missing member w/ default value" - missingMembersWithDefaultValue(foo, mapBasedFooDom)
        "missing member w/o default value" - missingMembersWithoutDefaultValue[Foo](mapBasedFooDom, "long")
        "duplicate member before fill completion" - dupMemberBeforeFillCompletion[Foo](mapBasedFooDom, 3, "short")
        "duplicate member after fill completion" - dupMemberAfterFillCompletion(foo, mapBasedFooDom, 5)
        "extra member before fill completion" - extraMemberBeforeFillCompletion(foo, mapBasedFooDom, 3)
        "extra member after fill completion" - extraMemberAfterFillCompletion(foo, mapBasedFooDom)
        "complex extra member before fill completion" - complexExtraMemberBeforeFC(foo, mapBasedFooDom, 3)
        "complex extra member after fill completion" - complexExtraMemberAfterFC(foo, mapBasedFooDom)
      }

      "Hundred" - {
        "size match" - sizeMatch(hundred, mapBased100DomNoDefaults, mapBased100Dom)
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

        implicit val qux0Codec = deriveCodec[Qux0]
        implicit val quxCodec  = deriveCodec[Qux]

        val qux0        = Qux0(42)
        val quxWithNone = Qux(42)
        val quxWithSome = Qux(42, Some(3.45))

        val quxWithSomeEncoded = encode(quxWithSome)
        decode[Qux](quxWithSomeEncoded) ==> quxWithSome

        val qux0Encoded        = encode(qux0)
        val quxWithNoneEncoded = encode(quxWithNone)

        qux0Encoded ==> quxWithNoneEncoded
        decode[Qux](quxWithNoneEncoded) ==> quxWithNone
      }

      "List with default value Nil" - {
        case class Qux0(int: Int)
        case class Qux(int: Int, optList: List[Float] = Nil)

        implicit val qux0Codec = deriveCodec[Qux0]
        implicit val quxCodec  = deriveCodec[Qux]

        val qux0       = Qux0(42)
        val quxWithNil = Qux(42)
        val quxWithNel = Qux(42, List(3.45f))

        val quxWithNelEncoded = encode(quxWithNel)
        decode[Qux](quxWithNelEncoded) ==> quxWithNel

        val qux0Encoded       = encode(qux0)
        val quxWithNilEncoded = encode(quxWithNil)

        qux0Encoded ==> quxWithNilEncoded
        decode[Qux](quxWithNilEncoded) ==> quxWithNil
      }

      "Recursive Case Class" - {
        case class Box(x: Option[Box] = None)
        object Box {
          implicit val codec: Codec[Box] = deriveCodec[Box]
        }
        roundTrip(recursiveBoxEncoded, Box(Some(Box(Some(Box())))))
      }

      "ADT" - {
        import ADT._

        try {
          implicit val dogCodec    = deriveCodec[Dog]
          implicit val catCodec    = deriveCodec[Cat]
          implicit val mouseCodec  = deriveCodec[Mouse]
          implicit val animalCodec = deriveCodec[Animal]

          val animals: List[Animal] = List(
            Dog(12, "Fred"),
            Cat(weight = 1.0, color = "none", home = "there"),
            Dog(4, "Lolle"),
            Mouse(true)
          )

          val encoded = encode(animals)
          decode[Element](encoded) ==> mapBasedAnimalsDom
          decode[List[Animal]](encoded) ==> animals
        } catch {
          case NonFatal(e) if target == Json =>
            e.getMessage ==> "JSON does not support integer values as a map key (Output.ToByteArray index 124)"
        }
      }

      "ADT Key Collision" - {
        import AdtWithKeyCollision._

        Scalac
          .typecheck("deriveEncoder[Animal]")
          .assertErrorMsgMatches(
            "@key collision: sub types `io.bullet.borer.derivation.AdtWithKeyCollision.Cat` and " +
              "`io.bullet.borer.derivation.AdtWithKeyCollision.Dog` of ADT " +
              "`io.bullet.borer.derivation.AdtWithKeyCollision.Animal` share the same type id `Dog`"
          )
      }

      "Diamond" - {
        sealed trait A
        sealed trait B       extends A
        sealed trait C       extends A
        case class D(a: Int) extends B with C

        implicit val d = deriveCodec[D]
        implicit val a = deriveCodec[A]
      }

      "Case Objects" - {
        sealed trait CaseObjectAdt
        case class Err(reason: String) extends CaseObjectAdt
        case object Ok                 extends CaseObjectAdt

        implicit val errCodec = deriveCodec[Err]
        implicit val okCodec  = deriveCodec[Ok.type]
        implicit val adtCodec = deriveCodec[CaseObjectAdt]

        val values: List[CaseObjectAdt] = List(Err("foo"), Ok)
        val encoded                     = encode(values)
        decode[Element](encoded) ==> mapBasedCaseObjectAdtDom
        decode[List[CaseObjectAdt]](encoded) ==> values
      }

      "Basic Type with custom Codec" - {
        case class Bar(i: Int, s: String)

        import Encoder.StringNumbers._
        import Decoder.StringNumbers._
        implicit val barCodec = deriveCodec[Bar]

        val bar     = Bar(42, "bar")
        val encoded = encode(bar)
        decode[Element](encoded) ==> mapBasedBarDom
        decode[Bar](encoded) ==> bar
      }
    }
  }

  def transform(elem: ArrayElem)(f: Vector[Element] => Vector[Element]): ArrayElem =
    elem match {
      case ArrayElem.Sized(x)   => ArrayElem.Sized(f(x))
      case ArrayElem.Unsized(x) => ArrayElem.Unsized(f(x))
    }

  def transform(elem: MapElem)(f: List[(Element, Element)] => List[(Element, Element)]): MapElem =
    elem match {
      case MapElem.Sized(_, keys, values)   => MapElem.Sized(f((keys zip values).toList): _*)
      case MapElem.Unsized(_, keys, values) => MapElem.Unsized(f((keys zip values).toList): _*)
      case _ => throw new IllegalStateException
    }
}

// cannot be moved into the DerivationSpec class itself due to
// https://github.com/scala/bug/issues/10035 not (yet) having been fixed in Scala 2.12
object ADT {
  sealed trait Animal
  case class Dog(age: Int, name: String)                                     extends Animal
  @key("TheCAT") case class Cat(weight: Double, color: String, home: String) extends Animal
  @key(42) case class Mouse(tail: Boolean)                                   extends Animal
}

object AdtWithKeyCollision {
  sealed trait Animal
  case class Dog(age: Int, name: String)                                  extends Animal
  @key("Dog") case class Cat(weight: Double, color: String, home: String) extends Animal
  @key(42) case class Mouse(tail: Boolean)                                extends Animal
}
