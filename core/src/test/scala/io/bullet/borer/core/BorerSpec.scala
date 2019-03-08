/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

import java.util
import utest._

abstract class BorerSpec[Bytes] extends TestSuite {

  implicit val byteAccess: ByteAccess[Bytes]
  def newInput(bytes: Array[Byte]): Input
  def outResultByteAccess: ByteAccess[byteAccess.Out#Result]

  def roundTrip[T: Encoder: Decoder](hexString: String, decodedValue: T): Unit =
    roundTrip(hexString, decodedValue, decodedValue)

  def roundTrip[A: Decoder, B: Encoder](hexString: String, decodedValue: A, encodedValue: B): Unit = {
    encode(encodedValue, hexString)
    decode(hexString, decodedValue)
  }

  def encode[T: Encoder](value: T, hexString: String): Unit = {
    val bytes          = hexBytes(hexString)
    val encodingResult = Cbor.encode(value).to[Bytes].bytes
    val encoded        = outResultByteAccess.toByteArray(encodingResult)
    if (!util.Arrays.equals(encoded, bytes)) {
      throw new java.lang.AssertionError(s"[$value] encodes to [${toHexString(encoded)}] rather than [$hexString]")
    }
  }

  def decode[T: Decoder](hexString: String, value: T): Unit = {
    val bytes   = hexBytes(hexString)
    val decoded = Cbor.decode(newInput(bytes)).to[T].value
    if (!equals(decoded, value)) {
      throw new java.lang.AssertionError(s"[$hexString] decodes to [$decoded] rather than [$value]")
    }
  }

  def equals[T](a: T, b: T): Boolean =
    (a, b) match {
      case (Float16(x), Float16(y)) if x.isNaN      ⇒ y.isNaN
      case (x: Float, y: Float) if x.isNaN          ⇒ y.isNaN
      case (x: Double, y: Double) if x.isNaN        ⇒ y.isNaN
      case (TaggedValue(tx, x), TaggedValue(ty, y)) ⇒ tx == ty && equals(x, y)
      case (x: Array[_], y: Array[_])               ⇒ x.toSeq == y.toSeq
      case _                                        ⇒ a == b
    }

  def toHexString(bytes: Array[Byte]): String = bytes.map("%02X" format _).mkString
  def hexBytes(hexString: String): Array[Byte] = {
    if ((hexString.length & 1) != 0) throw new IllegalArgumentException(s"[$hexString] is not a valid hex string")
    hexString.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
  }

}

object BorerSpec {

  trait ForByteArray { this: BorerSpec[Array[Byte]] ⇒
    val byteAccess                          = ByteAccess.ForByteArray
    def newInput(bytes: Array[Byte]): Input = bytes
    def outResultByteAccess                 = byteAccess
  }

  abstract class Default extends BorerSpec[Array[Byte]] with BorerSpec.ForByteArray
}
