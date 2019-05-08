/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import utest._

abstract class BorerSpec extends TestSuite {

  def encode[T: Encoder](value: T): String = toHexString(Cbor.encode(value).toByteArray)

  def decode[T: Decoder](encoded: String): T = Cbor.decode(hexBytes(encoded)).to[T].value

  final def roundTrip[T: Encoder: Decoder](encoded: String, decodedValue: T): Unit =
    roundTrip(encoded, decodedValue, decodedValue)

  final def roundTrip[A: Decoder, B: Encoder](encoded: String, decodedValue: A, encodedValue: B): Unit = {
    verifyEncoding(encodedValue, encoded)
    verifyDecoding(encoded, decodedValue)
  }

  final def verifyEncoding[T: Encoder](value: T, expectedEncoding: String): Unit = {
    val encoded = encode(value)
    if (encoded != expectedEncoding) {
      throw new java.lang.AssertionError(s"[$value] encodes to [$encoded] rather than [$expectedEncoding]")
    }
  }

  final def verifyDecoding[T: Decoder](encoded: String, expectedValue: T): Unit = {
    val decoded = decode(encoded)
    if (!equals(decoded, expectedValue)) {
      val msg = s"[$encoded] decodes to [${escape(decoded)}] rather than [${escape(expectedValue)}]"
      throw new java.lang.AssertionError(msg)
    }
  }

  final def equals[T](a: T, b: T): Boolean =
    (a, b) match {
      case (Float16(x), Float16(y)) if x.isNaN      => y.isNaN
      case (x: Float, y: Float) if x.isNaN          => y.isNaN
      case (x: Double, y: Double) if x.isNaN        => y.isNaN
      case (TaggedValue(tx, x), TaggedValue(ty, y)) => tx == ty && equals(x, y)
      case (x: Array[_], y: Array[_])               => x.toSeq == y.toSeq
      case _                                        => a == b
    }

  final def toHexString(bytes: Array[Byte]): String = bytes.map(x => f"${x & 0xFF}%02x").mkString

  final def hexBytes(hexString: String): Array[Byte] = {
    if ((hexString.length & 1) != 0) throw new IllegalArgumentException(s"[$hexString] is not a valid hex string")
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
