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

  def newOutput = new SomewhatImmutableByteArrayOutput(new Array[Byte](8), 0)

  def newInput(bytes: Array[Byte]) = new SomewhatImmutableByteArrayInput(bytes, 0, 0, Array.emptyByteArray)

  final class SomewhatImmutableByteArrayOutput(buffer: Array[Byte], val cursor: Int) extends Output[Array[Byte]] {

    type Self = SomewhatImmutableByteArrayOutput

    def writeByte(byte: Byte): SomewhatImmutableByteArrayOutput = {
      val newCursor = cursor + 1
      if (newCursor > 0) {
        val newBuffer = ensureLength(newCursor)
        newBuffer(cursor) = byte
        new SomewhatImmutableByteArrayOutput(newBuffer, newCursor)
      } else overflow()
    }

    def writeBytes(bytes: Array[Byte]): SomewhatImmutableByteArrayOutput = {
      val newCursor = cursor + bytes.length
      if (newCursor > 0) {
        val newBuffer = ensureLength(newCursor)
        System.arraycopy(bytes, 0, newBuffer, cursor, bytes.length)
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
      extends Input[Array[Byte]] {

    type Self = SomewhatImmutableByteArrayInput

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
