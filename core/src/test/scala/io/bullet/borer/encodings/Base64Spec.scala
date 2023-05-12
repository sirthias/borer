/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.encodings

import io.bullet.borer.BorerSuite
import io.bullet.borer.internal.Util._

class Base64Spec extends BorerSuite:

  test("rfc examples") {
    roundtrip("", "")
    roundtrip("f", "Zg==")
    roundtrip("fo", "Zm8=")
    roundtrip("foo", "Zm9v")
    roundtrip("foob", "Zm9vYg==")
    roundtrip("fooba", "Zm9vYmE=")
    roundtrip("foobar", "Zm9vYmFy")
  }

  test("roundtrip") {
    roundtrip(hex"", "")
    roundtrip(hex"01", "AQ==")
    roundtrip(hex"1234", "EjQ=")
    roundtrip(hex"234567", "I0Vn")
    roundtrip(hex"3456789a", "NFZ4mg==")
    roundtrip(hex"456789abcd", "RWeJq80=")
    roundtrip(hex"56789abcdef0", "VniavN7w")
    roundtrip(hex"6789abcdef0123", "Z4mrze8BIw==")
    roundtrip(hex"789abcdef0123456", "eJq83vASNFY=")
    roundtrip(hex"89abcdef0123456789", "iavN7wEjRWeJ")
    roundtrip(hex"9abcdef0123456789abc", "mrze8BI0VniavA==")
    roundtrip(hex"bcdef0123456789abcef01", "vN7wEjRWeJq87wE=")
  }

  def roundtrip(string: String, expectedEncoding: String): Unit =
    roundtrip(string getBytes "UTF8", expectedEncoding)

  def roundtrip(bytes: Array[Byte], expectedEncoding: String): Unit =
    new String(BaseEncoding.base64.encode(bytes)) ==> expectedEncoding
    BaseEncoding.base64.decode(expectedEncoding.toCharArray) ==> bytes
