/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer._
import io.bullet.borer.derivation.MapBasedCodecs.deriveCodec
import utest._

object CborFlatAdtEncodingSpec extends AbstractBorerSpec {

  def encode[T: Encoder](value: T): String =
    toHexString(Cbor.encode(value).withConfig(Cbor.EncodingConfig(bufferSize = 19)).toByteArray)

  def decode[T: Decoder](encoded: String): T = Cbor.decode(hexBytes(encoded)).to[T].value

  sealed trait Animal
  case class Dog(age: Int, name: String)                                     extends Animal
  @key("TheCAT") case class Cat(weight: Double, color: String, home: String) extends Animal
  @key(42) case class Mouse(tail: Boolean)                                   extends Animal
  case object Yeti                                                           extends Animal
  case class Fish(color: String)                                             extends Animal

  implicit val flatAdtEncoding = AdtEncodingStrategy.flat()

  implicit val dogCodec    = deriveCodec[Dog]
  implicit val catCodec    = deriveCodec[Cat]
  implicit val mouseCodec  = deriveCodec[Mouse]
  implicit val yetiCodec   = deriveCodec[Yeti.type]
  implicit val fishCodec   = ArrayBasedCodecs.deriveCodec[Fish]
  implicit val animalCodec = deriveCodec[Animal]

  val tests = Tests {

    "roundtrip" - {

      "default string type id" - {
        val dog = Dog(2, "Lolle")
        roundTrip("a26361676502646e616d65654c6f6c6c65", dog)
        roundTrip("a3655f7479706563446f676361676502646e616d65654c6f6c6c65", dog: Animal)
      }

      "custom string type id" - {
        val cat = Cat(5.6, "red", "street")
        roundTrip("a366776569676874fb401666666666666665636f6c6f726372656464686f6d6566737472656574", cat)
        roundTrip(
          "a4655f747970656654686543415466776569676874fb401666666666666665636f6c6f726372656464686f6d6566737472656574",
          cat: Animal)
      }

      "int type id" - {
        val mouse = Mouse(true)
        roundTrip("a1647461696cf5", mouse)
        roundTrip("a2655f74797065182a647461696cf5", mouse: Animal)
      }

      "case object" - {
        roundTrip("a0", Yeti)
        roundTrip("a1655f747970656459657469", Yeti: Animal)
      }

      "nested ADTs" - {
        sealed trait Result[+T]
        case class Good[+T](value: T) extends Result[T]
        case object Bad               extends Result[Nothing]

        implicit def goodCodec[T: Encoder: Decoder]   = deriveCodec[Good[T]]
        implicit val badCodec                         = deriveCodec[Bad.type]
        implicit def resultCodec[T: Encoder: Decoder] = deriveCodec[Result[T]]

        val goodDog = Good(Dog(2, "Lolle"))

        roundTrip(
          encode(
            Dom.MapElem.Sized(
              "value" -> Dom.MapElem.Sized("age" -> Dom.IntElem(2), "name" -> Dom.StringElem("Lolle")))),
          goodDog)

        roundTrip(
          encode(
            Dom.MapElem.Sized(
              "_type" -> Dom.StringElem("Good"),
              "value" -> Dom.MapElem
                .Sized("_type" -> Dom.StringElem("Dog"), "age" -> Dom.IntElem(2), "name" -> Dom.StringElem("Lolle")))),
          goodDog: Result[Animal])

        verifyDecoding(
          encode(
            Dom.MapElem.Sized(
              "value" -> Dom.MapElem
                .Sized("age" -> Dom.IntElem(2), "name" -> Dom.StringElem("Lolle"), "_type" -> Dom.StringElem("Dog")),
              "_type" -> Dom.StringElem("Good"))),
          goodDog: Result[Animal])
      }
    }

    "delayed type id member" - {

      "skipping simple members" - {
        verifyDecoding(
          encode(
            Dom.MapElem
              .Sized("age" -> Dom.IntElem(2), "_type" -> Dom.StringElem("Dog"), "name" -> Dom.StringElem("Lolle"))),
          Dog(2, "Lolle"): Animal)

        verifyDecoding(
          encode(
            Dom.MapElem
              .Sized("age" -> Dom.IntElem(2), "name" -> Dom.StringElem("Lolle"), "_type" -> Dom.StringElem("Dog"))),
          Dog(2, "Lolle"): Animal)
      }

      "skipping complex members" - {
        verifyDecoding(
          encode(
            Dom.MapElem.Sized(
              "age" -> Dom.IntElem(2),
              "ignored" -> Dom.ArrayElem.Sized(
                Dom.IntElem(1),
                Dom.ArrayElem.Sized(Dom.StringElem("a"), Dom.StringElem("b")),
                Dom.MapElem.Sized.empty),
              "_type" -> Dom.StringElem("Dog"),
              "name"  -> Dom.StringElem("Lolle")
            )
          ),
          Dog(2, "Lolle"): Animal)

        verifyDecoding(
          encode(
            Dom.MapElem.Sized(
              "name" -> Dom.StringElem("Lolle"),
              "age"  -> Dom.IntElem(2),
              "ignored" -> Dom.ArrayElem.Sized(
                Dom.IntElem(1),
                Dom.ArrayElem.Sized(Dom.StringElem("a"), Dom.StringElem("b")),
                Dom.MapElem.Sized.empty),
              "_type" -> Dom.StringElem("Dog"))
          ),
          Dog(2, "Lolle"): Animal)
      }
    }

    "non-map sub-type errors" - {
      val fish = Fish("red")
      roundTrip("63726564", fish)

      intercept[Borer.Error.Unsupported[_]] {
        verifyEncoding(fish: Animal, "{}")
      }.getMessage ==> "AdtEncodingStrategy.flat requires all sub-types of `io.bullet.borer.derivation.CborFlatAdtEncodingSpec.Animal` be serialized as a Map but here it was a String (Output.ToByteArray index 0)"

      intercept[Borer.Error.InvalidInputData[_]] {
        verifyDecoding("63726564", fish: Animal)
      }.getMessage ==> "Expected Map for decoding an instance of type `io.bullet.borer.derivation.CborFlatAdtEncodingSpec.Animal` but got none (input position 0)"

      intercept[Borer.Error.InvalidInputData[_]] {
        verifyDecoding("A0", fish: Animal)
      }.getMessage ==> "Expected Type-ID member `_type` for decoding an instance of type `io.bullet.borer.derivation.CborFlatAdtEncodingSpec.Animal` but got none (input position 0)"
    }
  }
}
