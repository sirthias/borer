/*
 * Copyright (c) 2019-2022 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import scala.collection.mutable.ListBuffer

class Utf8Spec extends BorerSuite:

  test("encode") {
    encode("")
    encode("f")
    encode("fo")
    encode("foo")
    encode("foob")
    encode("fooba")
    encode("foobar")
    encode("foobarq")
    encode("foobarqu")
    encode("foobarqux")

    encode("Weißabklärung")
    encode("árvíztűrő ütvefúrógép")
    encode("κόσμε")
  }

  test("decode") {
    decode("")
    decode("f")
    decode("fo")
    decode("foo")
    decode("foob")
    decode("fooba")
    decode("foobar")
    decode("foobarq")
    decode("foobarqu")
    decode("foobarqux")

    decode("Weißabklärung")
    decode("árvíztűrő ütvefúrógép")
    decode("κόσμε")
  }

  test("stress") {
    for (len <- 1 to stressTextA.length)
      encode(stressString(1, 0, len))
      encode(stressString(0, 1, len))
      encode(stressString(1, 1, len))
      encode(stressString(1, 2, len))
      encode(stressString(2, 1, len))
      encode(stressString(1, 3, len))
      encode(stressString(3, 1, len))
      encode(stressString(2, 2, len))
      encode(stressString(1, 4, len))
      encode(stressString(4, 1, len))

      decode(stressString(1, 0, len))
      decode(stressString(0, 1, len))
      decode(stressString(1, 1, len))
      decode(stressString(1, 2, len))
      decode(stressString(2, 1, len))
      decode(stressString(1, 3, len))
      decode(stressString(3, 1, len))
      decode(stressString(2, 2, len))
      decode(stressString(1, 4, len))
      decode(stressString(4, 1, len))
  }

  val stressTextA = "This is a text to somewhat stress the borer UTF8-Encoding/Decoding logic!!!"

  val stressTextB =
    "⊤\uD835\uDE5Dį\uD835\uDE34 ɨ\uD835\uDE00 \uD835\uDCEA ҭ\uD835\uDC1E\uD835\uDD35ȶ τớ \uD835\uDE34ơᵯȇẅẖẫ" +
      "\uD835\uDE69 \uD835\uDD64\uD835\uDE9Dᶉⅇ\uD835\uDD98\uD835\uDCC8 ƫ\uD835\uDDF5\uD835\uDCEE Ъ\uD835\uDCF8ᵲĕ" +
      "\uD835\uDD63 ÛΤ\uD835\uDDD98-⋿ꞑ\uD835\uDD20ǫժı\uD835\uDCF7\uD835\uDC20/\uD835\uDD6Féᴄō\uD835\uDD55\uD835" +
      "\uDC56п\uD835\uDE90 ȴỏġ\uD835\uDECA\uD801\uDC3D!!!"

  def stressString(a: Int, b: Int, len: Int) =
    val buf = new ListBuffer[Char]
    val bigChars =
      stressTextB
        .foldLeft(List.empty[List[Char]]) {
          case (List(x) :: tail, y) if Character.isHighSurrogate(x) => (x :: y :: Nil) :: tail
          case (acc, c)                                             => (c :: Nil) :: acc
        }
        .reverse
    stressTextA.zip(bigChars.take(len)).foldLeft(a -> b) {
      case ((1, 0), (c, _)) =>
        buf += c
        a -> b
      case ((0, 1), (_, cc)) =>
        buf ++= cc
        a -> b
      case ((0, x), (_, cc)) =>
        buf ++= cc
        0 -> (x - 1)
      case ((x, y), (c, _)) =>
        buf += c
        (x - 1) -> y
    }
    buf.mkString

  def encode(string: String): Unit =
    val result   = Utf8.encode(string.toCharArray)
    val expected = string getBytes "UTF8"
    result ==> expected

  def decode(string: String): Unit =
    val bytes    = string getBytes "UTF8"
    val result   = Utf8.decode(bytes)
    val expected = string.toCharArray
    result ==> expected
