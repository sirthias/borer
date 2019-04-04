/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.util

object ImmutableOutputRfcExamplesSpec extends AbstractRfcExamplesSpec("Immutable Output to Byte Array") {

  override def encode[T: Encoder](value: T): String =
    toHexString(Cbor.encode(value).to[Array[Byte]](byteAccess).bytes)

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

    def writeBytes(a: Byte, b: Byte)                   = writeByte(a).writeByte(b)
    def writeBytes(a: Byte, b: Byte, c: Byte)          = writeByte(a).writeByte(b).writeByte(c)
    def writeBytes(a: Byte, b: Byte, c: Byte, d: Byte) = writeByte(a).writeByte(b).writeByte(c).writeByte(d)

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

    private def overflow() = throw Borer.Error.Overflow(this, "Cannot output to byte array with > 2^31 bytes")
  }
}
