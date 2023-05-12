/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.output

import java.io.OutputStream

import io.bullet.borer.{ByteAccess, Output}
import io.bullet.borer.Output.ToValueProvider

trait ToOutputStreamOutput:

  implicit object ToOutputStreamProvider extends ToValueProvider[OutputStream]:
    type Out = ToOutputStream

    def apply(outputStream: OutputStream, bufferSize: Int, allowBufferCaching: Boolean) =
      new ToOutputStream(outputStream)

  abstract class ToOutputStreamBase(protected val outputStream: OutputStream) extends Output:
    type Self <: ToOutputStreamBase { type Self = ToOutputStreamBase.this.Self }

    final def writeByte(byte: Byte): Self =
      outputStream.write(byte.toInt)
      this.asInstanceOf[Self]

    final def writeBytes(a: Byte, b: Byte): Self          = writeByte(a).writeByte(b)
    final def writeBytes(a: Byte, b: Byte, c: Byte): Self = writeByte(a).writeByte(b).writeByte(c)

    final def writeBytes(a: Byte, b: Byte, c: Byte, d: Byte): Self =
      writeByte(a).writeByte(b).writeByte(c).writeByte(d)

    final def writeBytes[Bytes](bytes: Bytes)(implicit byteAccess: ByteAccess[Bytes]): Self =
      outputStream.write(byteAccess.toByteArray(bytes))
      this.asInstanceOf[Self]

  /**
   * Default, mutable implementation for serializing to [[OutputStream]] instances.
   *
   * NOTE: The given [[OutputStream]] is NOT closed at the end of the encoding run!
   */
  final class ToOutputStream(outputStream: OutputStream) extends ToOutputStreamBase(outputStream):
    type Self   = ToOutputStream
    type Result = OutputStream

    def result() = outputStream
