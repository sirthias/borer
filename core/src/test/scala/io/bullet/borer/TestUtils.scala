/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

trait TestUtils {

  final def toHexString(bytes: Array[Byte]): String = bytes.map(x => f"${x & 0xFF}%02x").mkString

  final def hexBytes(hexString: String): Array[Byte] = {
    if ((hexString.length & 1) != 0) throw new IllegalArgumentException(s"`$hexString` is not a valid hex string")
    hexString.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
  }

  final def escape(obj: Any): String =
    obj.toString.flatMap {
      case c if c >= ' ' => c.toString
      case '\b'          => "\\b"
      case '\f'          => "\\f"
      case '\n'          => "\\n"
      case '\t'          => "\\t"
      case '\r'          => "\\r"
      case c             => f"\\u${c.toInt}%04x"
    }
}
