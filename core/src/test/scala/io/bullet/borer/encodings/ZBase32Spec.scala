/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.encodings

import io.bullet.borer.{BorerSuite, TestUtils}
import io.bullet.borer.internal.Util.*

class ZBase32Spec extends BorerSuite with TestUtils:

  test("encode") {
    // spec examples
    encode(1, hex"00") ==> "y"
    encode(1, hex"80") ==> "o"
    encode(2, hex"40") ==> "e"
    encode(2, hex"C0") ==> "a"
    encode(10, hex"0000") ==> "yy"
    encode(10, hex"8080") ==> "on"
    encode(20, hex"8B8880") ==> "tqre"
    encode(20, hex"8B8880") ==> "tqre"
    encode(24, hex"F0BFC7") ==> "6n9hq"
    encode(24, hex"D47A04") ==> "4t7ye"
    encode(30, hex"F557BD0C") ==> "6im54d" // NOTE: the spec has a faulty value ("6im5sd") here!

    // other examples
    encode(0, hex"") ==> ""
    encode(8, hex"00") ==> "yy"
    encode(8, hex"FF") ==> "9h"
    encode(11, hex"FFE0") ==> "99o"
    encode(15, hex"8080") ==> "ony"
    encode(16, hex"8080") ==> "onyy"
    encode(20, hex"101110") ==> "nyet"
    encode(24, hex"101110") ==> "nyety"
    encode(40, hex"FFFFFFFFFF") ==> "99999999"
    encode(48, hex"FFFFFFFFFFFF") ==> "999999999h"
    encode(192, hex"c073624aaf3978514ef8443bb2a859c75fc3cc6af26d5aaa") ==> "ab3sr1ix8fhfnuzaeo75fkn3a7xh8udk6jsiiko"
  }

  test("decode") {
    // spec examples
    decode(1, "y") ==> "00"
    decode(1, "o") ==> "80"
    decode(2, "e") ==> "40"
    decode(2, "a") ==> "c0"
    decode(10, "yy") ==> "0000"
    decode(10, "on") ==> "8080"
    decode(20, "tqre") ==> "8b8880"
    decode(20, "tqre") ==> "8b8880"
    decode(24, "6n9hq") ==> "f0bfc7"
    decode(24, "4t7ye") ==> "d47a04"
    decode(30, "6im54d") ==> "f557bd0c" // NOTE: the spec has a faulty value ("6im5sd") here!

    // other examples
    decode(0, "") ==> ""
    decode(8, "yy") ==> "00"
    decode(8, "9h") ==> "ff"
    decode(11, "99o") ==> "ffe0"
    decode(15, "ony") ==> "8080"
    decode(16, "onyy") ==> "8080"
    decode(20, "nyet") ==> "101110"
    decode(24, "nyety") ==> "101110"
    decode(40, "99999999") ==> "ffffffffff"
    decode(48, "999999999h") ==> "ffffffffffff"
    decode(192, "ab3sr1ix8fhfnuzaeo75fkn3a7xh8udk6jsiiko") ==> "c073624aaf3978514ef8443bb2a859c75fc3cc6af26d5aaa"
  }

  test("roundtrip") {
    roundtrip(8, "b", "ce")
    roundtrip(16, "bo", "cjzo")
    roundtrip(24, "bor", "cjzzr")
    roundtrip(32, "bore", "cjzzr3e")
    roundtrip(40, "borer", "cjzzr3m1")
    roundtrip(48, "borer ", "cjzzr3m1ry")
    roundtrip(56, "borer r", "cjzzr3m1rb3y")
    roundtrip(64, "borer ro", "cjzzr3m1rb3g6")
    roundtrip(72, "borer rox", "cjzzr3m1rb3g66y")
    roundtrip(80, "borer rox!", "cjzzr3m1rb3g66bb")
    roundtrip(88, "borer rox!!", "cjzzr3m1rb3g66bbrr")

    roundtrip(
      776,
      "And, of course, the famous Hungarian flood-resistant hammerdrill: \"árvíztűrő ütvefúrógép\"",
      "efzgembyp7unya5xqi38g3jcrb4go3jyc3os455iqcoro7mqc7ozr4mbpaogc5dxp71n4hufqpwzg7dbp34ny4dbpisskhurqjwsa5b4rytc8em1q5b446uwasazftctrdb5a7dsciuc8qu1aq3sxo7jqyty")
  }

  test("produce proper error messages") {
    intercept[IllegalArgumentException](
      decode(24, "nope!")
    ).getMessage ==> "\"nope!\" is not a valid z-base32 encoding. '!' at index 4 is not part of the z-base32 alphabet."
  }

  def encode(bitCount: Long, bytes: Array[Byte]): String =
    new String(BaseEncoding.zbase32.encode(bitCount, bytes))

  def decode(bitCount: Long, string: String): String =
    toHexString(BaseEncoding.zbase32.decode(bitCount, string.toCharArray))

  def roundtrip(bitCount: Long, string: String, expectedEncoding: String): Unit =
    encode(bitCount, string.getBytes("UTF8")) ==> expectedEncoding
    new String(BaseEncoding.zbase32.decode(bitCount, expectedEncoding.toCharArray), "UTF8") ==> string
