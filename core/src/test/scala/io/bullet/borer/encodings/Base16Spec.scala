/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.encodings

import io.bullet.borer.internal.Util._
import io.bullet.borer.TestUtils
import utest._

object Base16Spec extends TestSuite with TestUtils {

  val tests = Tests {

    "rfc examples" - {
      roundtrip("", "")
      roundtrip("f", "66")
      roundtrip("fo", "666f")
      roundtrip("foo", "666f6f")
      roundtrip("foob", "666f6f62")
      roundtrip("fooba", "666f6f6261")
      roundtrip("foobar", "666f6f626172")
    }

    "roundtrip" - {
      roundtrip(hex"", "")
      roundtrip(hex"01", "01")
      roundtrip(hex"1234", "1234")
      roundtrip(hex"234567", "234567")
      roundtrip(hex"3456789a", "3456789a")
      roundtrip(hex"456789abcd", "456789abcd")
      roundtrip(hex"56789abcdef0", "56789abcdef0")
      roundtrip(hex"6789abcdef0123", "6789abcdef0123")
      roundtrip(hex"789abcdef0123456", "789abcdef0123456")
      roundtrip(hex"89abcdef0123456789", "89abcdef0123456789")
      roundtrip(hex"9abcdef0123456789abc", "9abcdef0123456789abc")
    }

  }

  def roundtrip(string: String, expectedEncoding: String): Unit =
    roundtrip(string getBytes "UTF8", expectedEncoding)

  def roundtrip(bytes: Array[Byte], expectedEncoding: String): Unit = {
    new String(Base16.encode(bytes)) ==> expectedEncoding
    Base16.decode(expectedEncoding.toCharArray) ==> bytes
  }
}
