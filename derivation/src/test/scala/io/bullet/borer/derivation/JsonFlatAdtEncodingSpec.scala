/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import java.nio.charset.StandardCharsets

import io.bullet.borer._

class JsonFlatAdtEncodingSpec extends AbstractBorerSpec {

  def encode[T: Encoder](value: T): String = Json.encode(value).toUtf8String

  def decode[T: Decoder](encoded: String): T =
    Json.decode(encoded getBytes StandardCharsets.UTF_8).to[T].value

  sealed trait Animal
  case class Dog(age: Int, name: String)                                     extends Animal
  @key("TheCAT") case class Cat(weight: Double, color: String, home: String) extends Animal
  @key(42) case class Mouse(tail: Boolean)                                   extends Animal
  case object Yeti                                                           extends Animal
  case class Fish(color: String)                                             extends Animal

  given AdtEncodingStrategy = AdtEncodingStrategy.flat()

  given Codec[Dog]       = MapBasedCodecs.deriveCodec[Dog]
  given Codec[Cat]       = MapBasedCodecs.deriveCodec[Cat]
  given Codec[Mouse]     = MapBasedCodecs.deriveCodec[Mouse]
  given Codec[Yeti.type] = MapBasedCodecs.deriveCodec[Yeti.type]
  given Codec[Fish]      = ArrayBasedCodecs.deriveCodec[Fish]
  given Codec[Animal]    = MapBasedCodecs.deriveCodec[Animal]

  test("<roundtrip> default string type id") {
    val dog = Dog(2, "Lolle")
    roundTrip("""{"age":2,"name":"Lolle"}""", dog)
    roundTrip("""{"_type":"Dog","age":2,"name":"Lolle"}""", dog: Animal)
  }

  test("<roundtrip> custom string type id") {
    val cat = Cat(5.6, "red", "street")
    roundTrip("""{"weight":5.6,"color":"red","home":"street"}""", cat)
    roundTrip("""{"_type":"TheCAT","weight":5.6,"color":"red","home":"street"}""", cat: Animal)
  }

  test("<roundtrip> int type id") {
    val mouse = Mouse(true)
    roundTrip("""{"tail":true}""", mouse)
    roundTrip("""{"_type":42,"tail":true}""", mouse: Animal)
  }

  test("<roundtrip> case object") {
    roundTrip("""{}""", Yeti)
    roundTrip("""{"_type":"Yeti"}""", Yeti: Animal)
  }

  test("<roundtrip> nested ADTs") {
    sealed trait Result[+T]
    case class Good[+T](value: T) extends Result[T]
    case object Bad               extends Result[Nothing]

    given [T: Encoder: Decoder]: Codec[Good[T]]   = MapBasedCodecs.deriveCodec
    given Codec[Bad.type]                         = MapBasedCodecs.deriveCodec
    given [T: Encoder: Decoder]: Codec[Result[T]] = MapBasedCodecs.deriveCodec

    val goodDog = Good(Dog(2, "Lolle"))
    roundTrip("""{"value":{"age":2,"name":"Lolle"}}""", goodDog)
    roundTrip("""{"_type":"Good","value":{"_type":"Dog","age":2,"name":"Lolle"}}""", goodDog: Result[Animal])
    verifyDecoding("""{"value":{"age":2,"_type":"Dog","name":"Lolle"}, "_type":"Good"}""", goodDog: Result[Animal])

    val dogs = Good(List(Dog(2, "LL")))
    roundTrip("""{"value":[{"age":2,"name":"LL"}]}""", dogs)
    roundTrip("""{"_type":"Good","value":[{"_type":"Dog","age":2,"name":"LL"}]}""", dogs: Result[List[Animal]])
    verifyDecoding("""{"value":[{"age":2,"_type":"Dog","name":"LL"}],"_type":"Good"}""", dogs: Result[List[Animal]])
  }

  test("<roundtrip> in list") {
    val animals: List[Animal] = List(Dog(2, "Lolle"))
    roundTrip("""[{"_type":"Dog","age":2,"name":"Lolle"}]""", animals)
  }

  test("<roundtrip> stacked") {
    sealed trait A
    sealed trait B             extends A
    case class C(x: Option[B]) extends B

    given Codec[B] = MapBasedCodecs.deriveAllCodecs[B]
    given Codec[A] = MapBasedCodecs.deriveCodec[A]

    roundTrip("""{"_type":"C","x":[]}""", C(None): A)
  }

  test("skipping simple members") {
    verifyDecoding("""{"age":2,"_type":"Dog","name":"Lolle"}""", Dog(2, "Lolle"): Animal)
    verifyDecoding("""{"age":2,"name":"Lolle","_type":"Dog"}""", Dog(2, "Lolle"): Animal)
  }

  test("skipping complex members") {
    verifyDecoding("""{"age":2,"ignored":[1,["a","b"],{}],"_type":"Dog","name":"Lolle"}""", Dog(2, "Lolle"): Animal)
    verifyDecoding("""{"age":2,"ignored":[1,["a","b"],{}],"name":"Lolle","_type":"Dog"}""", Dog(2, "Lolle"): Animal)
  }

  test("non-map sub-type errors") {
    val fish = Fish("red")
    roundTrip(""""red"""", fish)

    intercept[Borer.Error.Unsupported[_]] {
      verifyEncoding(fish: Animal, "{}")
    }.getMessage ==> "AdtEncodingStrategy.flat requires all sub-types of `Animal` be serialized as a Map but here it was a String (Output.ToByteArray index 0)"

    intercept[Borer.Error.InvalidInputData[_]] {
      verifyDecoding(""""red"""", fish: Animal)
    }.getMessage ==> "Expected Map for decoding an instance of type `Animal` but got Chars (input position 0)"

    intercept[Borer.Error.InvalidInputData[_]] {
      verifyDecoding("{}", fish: Animal)
    }.getMessage ==> "Expected Type-ID member `_type` for decoding an instance of type `Animal` but got none (input position 1)"
  }
}
