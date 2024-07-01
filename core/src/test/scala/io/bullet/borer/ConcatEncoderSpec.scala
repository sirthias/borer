/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

class ConcatEncoderSpec extends ByteArrayCborSpec:
  import Dom._

  test("bounded array + bounded array") {
    implicit val encoder: Encoder[(Array[Int], Vector[String])] = concatTupleEncoder[Array[Int], Vector[String]]
    roundTrip(
      "85010203636162636378797a",
      ArrayElem.Sized(IntElem(1), IntElem(2), IntElem(3), StringElem("abc"), StringElem("xyz")),
      (Array(1, 2, 3), Vector("abc", "xyz"))
    )
  }

  test("bounded array + unbounded array") {
    implicit val encoder: Encoder[(Array[Int], List[String])] = concatTupleEncoder[Array[Int], List[String]]
    roundTrip(
      "9f010203636162636378797aff",
      ArrayElem.Unsized(IntElem(1), IntElem(2), IntElem(3), StringElem("abc"), StringElem("xyz")),
      (Array(1, 2, 3), List("abc", "xyz"))
    )
  }

  test("unbounded array + bounded array") {
    implicit val encoder: Encoder[(List[Int], Array[String])] = concatTupleEncoder[List[Int], Array[String]]
    roundTrip(
      "9f010203636162636378797aff",
      ArrayElem.Unsized(IntElem(1), IntElem(2), IntElem(3), StringElem("abc"), StringElem("xyz")),
      (List(1, 2, 3), Array("abc", "xyz"))
    )
  }

  test("unbounded array + unbounded array") {
    implicit val encoder: Encoder[(List[Int], List[String])] = concatTupleEncoder[List[Int], List[String]]
    roundTrip(
      "9f010203636162636378797aff",
      ArrayElem.Unsized(IntElem(1), IntElem(2), IntElem(3), StringElem("abc"), StringElem("xyz")),
      (List(1, 2, 3), List("abc", "xyz"))
    )
  }

  test("bounded map + bounded map") {
    implicit val encoder: Encoder[(Map[String, Int], Map[String, String])] =
      concatTupleEncoder[Map[String, Int], Map[String, String]]
    roundTrip(
      "a561610161620261630361786361626361796378797a",
      MapElem.Sized(
        "a" -> IntElem(1),
        "b" -> IntElem(2),
        "c" -> IntElem(3),
        "x" -> StringElem("abc"),
        "y" -> StringElem("xyz")),
      (Map("a" -> 1, "b" -> 2, "c" -> 3), Map("x" -> "abc", "y" -> "xyz"))
    )
  }

  test("bounded map + unbounded map") {
    implicit val encoder: Encoder[(Map[String, Int], Writer.Script)] =
      concatTupleEncoder[Map[String, Int], Writer.Script]
    val script = Writer.Script(_.writeMapStart().~("x").~("abc").~("y").~("xyz").writeBreak())
    roundTrip(
      "bf61610161620261630361786361626361796378797aff",
      MapElem.Unsized(
        "a" -> IntElem(1),
        "b" -> IntElem(2),
        "c" -> IntElem(3),
        "x" -> StringElem("abc"),
        "y" -> StringElem("xyz")),
      (Map("a" -> 1, "b" -> 2, "c" -> 3), script)
    )
  }

  test("unbounded map + bounded map") {
    implicit val encoder: Encoder[(Writer.Script, Map[String, Int])] =
      concatTupleEncoder[Writer.Script, Map[String, Int]]
    val script = Writer.Script(_.writeMapStart().~("x").~("abc").~("y").~("xyz").writeBreak())
    roundTrip(
      "bf61786361626361796378797a616101616202616303ff",
      MapElem.Unsized(
        "x" -> StringElem("abc"),
        "y" -> StringElem("xyz"),
        "a" -> IntElem(1),
        "b" -> IntElem(2),
        "c" -> IntElem(3)),
      (script, Map("a" -> 1, "b" -> 2, "c" -> 3))
    )
  }

  test("unbounded map + unbounded map") {
    implicit val encoder: Encoder[(Writer.Script, Writer.Script)] = concatTupleEncoder[Writer.Script, Writer.Script]
    roundTrip(
      "bf61610161620261630361786361626361796378797aff",
      MapElem.Unsized(
        "a" -> IntElem(1),
        "b" -> IntElem(2),
        "c" -> IntElem(3),
        "x" -> StringElem("abc"),
        "y" -> StringElem("xyz")),
      (
        Writer.Script(_.writeMapStart().~("a").~(1).~("b").~(2).~("c").~(3).writeBreak()),
        Writer.Script(_.writeMapStart().~("x").~("abc").~("y").~("xyz").writeBreak()))
    )
  }

  test("array + map") {
    implicit val encoder: Encoder[(Array[Int], Map[String, Int])] = concatTupleEncoder[Array[Int], Map[String, Int]]
    intercept[Borer.Error.Unsupported[_]](
      encode(Array(1, 2, 3) -> Map("a" -> 1, "b" -> 2, "c" -> 3))
    ).getMessage ==> "Cannot merge a 'to-Array' Encoder with a 'to-Map' Encoder (Output.ToByteArray index 0)"
  }

  test("map + array") {
    implicit val encoder: Encoder[(Map[String, Int], Array[Int])] = concatTupleEncoder[Map[String, Int], Array[Int]]
    intercept[Borer.Error.Unsupported[_]](
      encode(Map("a" -> 1, "b" -> 2, "c" -> 3) -> Array(1, 2, 3))
    ).getMessage ==> "Cannot merge a 'to-Map' Encoder with a 'to-Array' Encoder (Output.ToByteArray index 0)"
  }

  test("array + string") {
    implicit val encoder: Encoder[(Array[Int], String)] = concatTupleEncoder[Array[Int], String]
    intercept[Borer.Error.Unsupported[_]](
      encode(Array(1, 2, 3) -> "nope")
    ).getMessage ==> "Second Encoder produced the String `nope` but Encoder merging only supports 'to-Array' and 'to-Map' Encoders (Output.ToByteArray index 0)"
  }

  test("string + array") {
    implicit val encoder: Encoder[(String, Array[Int])] = concatTupleEncoder[String, Array[Int]]
    intercept[Borer.Error.Unsupported[_]](
      encode("nope" -> Array(1, 2, 3))
    ).getMessage ==> "First Encoder produced the String `nope` but Encoder merging only supports 'to-Array' and 'to-Map' Encoders (Output.ToByteArray index 0)"
  }

  def concatTupleEncoder[A: Encoder, B: Encoder]: Encoder[(A, B)] = select1stEncoder[A, B] concat select2ndEncoder[A, B]

  def select1stEncoder[A: Encoder, B]: Encoder[(A, B)] = Encoder((w, t) => w ~ t._1)
  def select2ndEncoder[A, B: Encoder]: Encoder[(A, B)] = Encoder((w, t) => w ~ t._2)
