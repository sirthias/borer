/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.output

import io.bullet.borer.{ByteAccess, Output}
import io.bullet.borer.Output.ToTypeProvider

trait ToUnitOutput:

  /**
   * Simple NOP output that doesn't actually write anything and always produces `Unit`.
   * Useful for running an encoding purely for its side effects, e.g. logging or validation.
   */
  implicit object ToUnitProvider extends ToTypeProvider[Unit] with Output:
    type Out = this.type
    def apply(bufferSize: Int, allowBufferCaching: Boolean) = this

    type Self   = this.type
    type Result = Unit

    def writeByte(byte: Byte)                          = this
    def writeBytes(a: Byte, b: Byte)                   = this
    def writeBytes(a: Byte, b: Byte, c: Byte)          = this
    def writeBytes(a: Byte, b: Byte, c: Byte, d: Byte) = this
    def writeBytes[Bytes: ByteAccess](bytes: Bytes)    = this

    def result(): Unit = ()
