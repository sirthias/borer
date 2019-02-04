/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

import java.util

import io.bullet.borer.core
import utest._

abstract class BorerSpec[Bytes](implicit byteAccess: ByteAccess[Bytes]) extends TestSuite {

  def newOutput: Output[Bytes]
  def newInput(bytes: Array[Byte]): Input[Bytes]

  def roundTrip[T](hexString: String, decodedValue: T)(implicit e: Encoder[Bytes, T], d: Decoder[Bytes, T]): Unit =
    roundTrip(hexString, decodedValue, decodedValue)

  def roundTrip[A, B](hexString: String, decodedValue: A, encodedValue: B)(implicit d: Decoder[Bytes, A],
                                                                           e: Encoder[Bytes, B]): Unit = {
    encode(encodedValue, hexString)
    decode(hexString, decodedValue)
  }

  def encode[T](value: T, hexString: String)(implicit e: Encoder[Bytes, T]): Unit = {
    val bytes          = hexBytes(hexString)
    val encodingResult = Cbor.generalEncode(value, newOutput).fold(throw _, identity)
    val encoded        = byteAccess.toByteArray(encodingResult.result())
    if (!util.Arrays.equals(encoded, bytes)) {
      throw new java.lang.AssertionError(s"[$value] encodes to [${toHexString(encoded)}] rather than [$hexString]")
    }
  }

  def decode[T](hexString: String, value: T)(implicit d: Decoder[Bytes, T]): Unit = {
    val bytes   = hexBytes(hexString)
    val decoded = Cbor.decode[T].from[Bytes](newInput(bytes)).fold(throw _, _._1)
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

  trait DefaultBytes { this: BorerSpec[Array[Byte]] ⇒
    def newOutput                    = new core.Output.ToByteArray
    def newInput(bytes: Array[Byte]) = new Input.FromByteArray(bytes)
  }

  abstract class Default extends BorerSpec[Array[Byte]] with BorerSpec.DefaultBytes
}
