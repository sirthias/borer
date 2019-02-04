/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

/**
  * Type class for providing basic access to a custom `Bytes` abstraction.
  */
trait ByteAccess[Bytes] {

  def sizeOf(bytes: Bytes): Long

  def fromByteArray(byteArray: Array[Byte]): Bytes

  def toByteArray(bytes: Bytes): Array[Byte]

  def concat(a: Bytes, b: Bytes): Bytes

  def empty: Bytes
}

object ByteAccess {

  /**
    * The default [[ByteAccess]] for plain byte arrays.
    */
  implicit val forByteArray: ByteAccess[Array[Byte]] =
    new ByteAccess[Array[Byte]] {

      def sizeOf(bytes: Array[Byte]): Long = bytes.length.toLong

      def fromByteArray(byteArray: Array[Byte]): Array[Byte] = byteArray

      def toByteArray(bytes: Array[Byte]): Array[Byte] = bytes

      def concat(a: Array[Byte], b: Array[Byte]) =
        if (a.length > 0) {
          if (b.length > 0) {
            val len = a.length + b.length
            if (len >= 0) {
              val result = new Array[Byte](len)
              System.arraycopy(a, 0, result, 0, a.length)
              System.arraycopy(b, 0, result, a.length, b.length)
              result
            } else sys.error("Cannot concatenate two byte arrays with a total size > 2^31 bytes")
          } else a
        } else b

      val empty = Array.emptyByteArray
    }
}
