/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.encodings

import io.bullet.borer.BorerSuite
import io.bullet.borer.internal.Util._

class Base32Spec extends BorerSuite:

  test("rfc examples") {
    roundtrip("", "")
    roundtrip("f", "MY======")
    roundtrip("fo", "MZXQ====")
    roundtrip("foo", "MZXW6===")
    roundtrip("foob", "MZXW6YQ=")
    roundtrip("fooba", "MZXW6YTB")
    roundtrip("foobar", "MZXW6YTBOI======")
  }

  test("roundtrip") {
    roundtrip(hex"", "")
    roundtrip(hex"01", "AE======")
    roundtrip(hex"1234", "CI2A====")
    roundtrip(hex"234567", "ENCWO===")
    roundtrip(hex"3456789a", "GRLHRGQ=")
    roundtrip(hex"456789abcd", "IVTYTK6N")
    roundtrip(hex"56789abcdef0", "KZ4JVPG66A======")
    roundtrip(hex"6789abcdef0123", "M6E2XTPPAERQ====")
    roundtrip(hex"789abcdef0123456", "PCNLZXXQCI2FM===")
    roundtrip(hex"89abcdef0123456789", "RGV433YBENCWPCI=")
    roundtrip(hex"9abcdef0123456789abc", "TK6N54ASGRLHRGV4")
    roundtrip(hex"bcdef0123456789abcef01", "XTPPAERUKZ4JVPHPAE======")
  }

  def roundtrip(string: String, expectedEncoding: String): Unit =
    roundtrip(string getBytes "UTF8", expectedEncoding)

  def roundtrip(bytes: Array[Byte], expectedEncoding: String): Unit =
    new String(BaseEncoding.base32.encode(bytes)) ==> expectedEncoding
    BaseEncoding.base32.decode(expectedEncoding.toCharArray) ==> bytes
