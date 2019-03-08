/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

import java.util

object ImmutableRfcExamplesSpec extends AbstractRfcExamplesSpec[Array[Byte]]("Immutable Byte Array") {

  def newInput(bytes: Array[Byte]) = new SomewhatImmutableByteArrayInput(bytes, 0, 0, Array.emptyByteArray)
  def outResultByteAccess          = byteAccess

  object byteAccess extends ByteAccess[Array[Byte]] {
    type Out = SomewhatImmutableByteArrayOutput

    def newOutput = new SomewhatImmutableByteArrayOutput(new Array[Byte](8), 0)

    def sizeOf(bytes: Array[Byte])                               = ByteAccess.ForByteArray.sizeOf(bytes)
    def fromByteArray(byteArray: Array[Byte])                    = ByteAccess.ForByteArray.fromByteArray(byteArray)
    def toByteArray(bytes: Array[Byte])                          = ByteAccess.ForByteArray.toByteArray(bytes)
    def concat(a: Array[Byte], b: Array[Byte])                   = ByteAccess.ForByteArray.concat(a, b)
    def convert[B](value: B)(implicit byteAccess: ByteAccess[B]) = ByteAccess.ForByteArray.convert(value)
    def empty                                                    = ByteAccess.ForByteArray.empty
  }

  final class SomewhatImmutableByteArrayOutput(buffer: Array[Byte], val cursor: Int) extends Output {

    type Self   = SomewhatImmutableByteArrayOutput
    type Result = Array[Byte]

    def writeByte(byte: Byte): SomewhatImmutableByteArrayOutput = {
      val newCursor = cursor + 1
      if (newCursor > 0) {
        val newBuffer = ensureLength(newCursor)
        newBuffer(cursor) = byte
        new SomewhatImmutableByteArrayOutput(newBuffer, newCursor)
      } else overflow()
    }

    def writeBytes[Bytes](bytes: Bytes)(implicit ba: ByteAccess[Bytes]): SomewhatImmutableByteArrayOutput = {
      val byteArray = ba.toByteArray(bytes)
      val newCursor = cursor + byteArray.length
      if (newCursor > 0) {
        val newBuffer = ensureLength(newCursor)
        System.arraycopy(byteArray, 0, newBuffer, cursor, byteArray.length)
        new SomewhatImmutableByteArrayOutput(newBuffer, newCursor)
      } else overflow()
    }

    def result(): Array[Byte] =
      if (cursor < buffer.length) {
        val result = new Array[Byte](cursor)
        System.arraycopy(buffer, 0, result, 0, cursor)
        result
      } else buffer

    private def ensureLength(minSize: Int): Array[Byte] =
      if (buffer.length < minSize) {
        val newLen = math.max(buffer.length << 1, minSize)
        util.Arrays.copyOf(buffer, newLen)
      } else buffer

    private def overflow() = throw new Cbor.Error.Overflow(this, "Cannot output to byte array with > 2^31 bytes")
  }

  final class SomewhatImmutableByteArrayInput(buffer: Array[Byte],
                                              val cursor: Int,
                                              val lastByte: Byte,
                                              val lastBytes: Array[Byte])
      extends Input {

    type Self  = SomewhatImmutableByteArrayInput
    type Bytes = Array[Byte]

    def byteAccess = ByteAccess.ForByteArray

    def hasBytes(length: Long): Boolean = {
      val off = length + cursor
      0 <= off && off <= buffer.length
    }

    def readByte(): Self = new SomewhatImmutableByteArrayInput(buffer, cursor + 1, buffer(cursor), lastBytes)

    def readBytes(length: Long): Self =
      if (length >> 31 == 0) {
        if (length > 0) {
          val len          = length.toInt
          val newLastBytes = new Array[Byte](len)
          System.arraycopy(buffer, cursor, newLastBytes, 0, len)
          new SomewhatImmutableByteArrayInput(buffer, cursor + len, lastByte, newLastBytes)
        } else if (lastBytes.length != 0) {
          new SomewhatImmutableByteArrayInput(buffer, cursor, lastByte, Array.emptyByteArray)
        } else this
      } else throw new Cbor.Error.Overflow(cursor, "Byte-array input is limited to size 2GB")

    def copy: SomewhatImmutableByteArrayInput = this
  }
}
