/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.encodings

import io.bullet.borer.TestUtils
import utest._

object Base32HexSpec extends TestSuite with TestUtils:

  val tests = Tests {

    "rfc examples" - {
      roundtrip("", "")
      roundtrip("f", "CO======")
      roundtrip("fo", "CPNG====")
      roundtrip("foo", "CPNMU===")
      roundtrip("foob", "CPNMUOG=")
      roundtrip("fooba", "CPNMUOJ1")
      roundtrip("foobar", "CPNMUOJ1E8======")
    }
  }

  def roundtrip(string: String, expectedEncoding: String): Unit =
    roundtrip(string getBytes "UTF8", expectedEncoding)

  def roundtrip(bytes: Array[Byte], expectedEncoding: String): Unit =
    new String(BaseEncoding.base32hex.encode(bytes)) ==> expectedEncoding
    BaseEncoding.base32hex.decode(expectedEncoding.toCharArray) ==> bytes
