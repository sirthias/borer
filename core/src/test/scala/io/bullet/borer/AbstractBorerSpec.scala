/*
 * Copyright (c) 2019-2022 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import utest._

abstract class AbstractBorerSpec extends TestSuite with TestUtils {

  def encode[T: Encoder](value: T): String

  def decode[T: Decoder](encoded: String): T

  final def roundTrip[T: Encoder: Decoder](encoded: String, decodedValue: T): Unit =
    roundTrip(encoded, decodedValue, decodedValue)

  final def roundTrip[A: Decoder, B: Encoder](encoded: String, decodedValue: A, encodedValue: B): Unit = {
    verifyEncoding(encodedValue, encoded)
    verifyDecoding(encoded, decodedValue)
  }

  final def verifyEncoding[T: Encoder](value: T, expectedEncoding: String): Unit = {
    val encoded = encode(value)
    if (encoded != expectedEncoding) {
      throw new java.lang.AssertionError(s"`$value` encodes to `$encoded` rather than `$expectedEncoding`")
    }
  }

  final def verifyDecoding[T: Decoder](encoded: String, expectedValue: T): Unit = {
    val decoded = decode(encoded)
    if (!equals(decoded, expectedValue)) {
      val msg = s"`$encoded` decodes to `${escape(decoded)}` rather than `${escape(expectedValue)}`"
      throw new java.lang.AssertionError(msg)
    }
  }

  final def equals[T](a: T, b: T): Boolean =
    (a, b) match {
      case (Float16(x), Float16(y)) if x.isNaN      => y.isNaN
      case (x: Float, y: Float) if x.isNaN          => y.isNaN
      case (x: Double, y: Double) if x.isNaN        => y.isNaN
      case (TaggedValue(tx, x), TaggedValue(ty, y)) => tx == ty && equals(x, y)
      case (x: Array[_], y: Array[_])               => equals(x.toSeq, y.toSeq)
      case (x: Seq[_], y: Seq[_])                   => x.size == y.size && x.zip[Any](y).forall(t => equals(t._1, t._2))
      case _                                        => a == b
    }
}
