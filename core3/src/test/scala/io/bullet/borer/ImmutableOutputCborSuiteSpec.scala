/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.util

class ImmutableOutputCborSuiteSpec extends AbstractCborSuiteSpec:

  def encode[T: Encoder](value: T): String   = toHexString(Cbor.encode(value).to[Array[Byte]].result)
  def decode[T: Decoder](encoded: String): T = Cbor.decode(hexBytes(encoded)).to[T].value

  implicit object SomewhatImmutableByteArrayOutputProvider extends Output.ToTypeProvider[Array[Byte]]:
    type Out = SomewhatImmutableByteArrayOutput

    def apply(bufferSize: Int, allowBufferCaching: Boolean) =
      new SomewhatImmutableByteArrayOutput(new Array[Byte](8), 0)

  final class SomewhatImmutableByteArrayOutput(buffer: Array[Byte], val cursor: Int) extends Output:
    type Self   = SomewhatImmutableByteArrayOutput
    type Result = Array[Byte]

    def writeByte(byte: Byte): SomewhatImmutableByteArrayOutput =
      val newCursor = cursor + 1
      if (newCursor > 0)
        val newBuffer = ensureLength(newCursor)
        newBuffer(cursor) = byte
        new SomewhatImmutableByteArrayOutput(newBuffer, newCursor)
      else overflow()

    def writeBytes(a: Byte, b: Byte)                   = writeByte(a).writeByte(b)
    def writeBytes(a: Byte, b: Byte, c: Byte)          = writeByte(a).writeByte(b).writeByte(c)
    def writeBytes(a: Byte, b: Byte, c: Byte, d: Byte) = writeByte(a).writeByte(b).writeByte(c).writeByte(d)

    def writeBytes[Bytes](bytes: Bytes)(implicit ba: ByteAccess[Bytes]): SomewhatImmutableByteArrayOutput =
      val byteArray = ba.toByteArray(bytes)
      val newCursor = cursor + byteArray.length
      if (newCursor > 0)
        val newBuffer = ensureLength(newCursor)
        System.arraycopy(byteArray, 0, newBuffer, cursor, byteArray.length)
        new SomewhatImmutableByteArrayOutput(newBuffer, newCursor)
      else overflow()

    def result(): Array[Byte] =
      if (cursor < buffer.length)
        val result = new Array[Byte](cursor)
        System.arraycopy(buffer, 0, result, 0, cursor)
        result
      else buffer

    private def ensureLength(minSize: Int): Array[Byte] =
      if (buffer.length < minSize)
        val newLen = math.max(buffer.length << 1, minSize)
        util.Arrays.copyOf(buffer, newLen)
      else buffer

    private def overflow() = throw new Borer.Error.Overflow(this, "Cannot output to byte array with > 2^31 bytes")
