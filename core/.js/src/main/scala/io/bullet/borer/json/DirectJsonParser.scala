/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.json

import io.bullet.borer.internal.Parser
import io.bullet.borer.Receiver

// never actually instantiated anywhere
final private[borer] class DirectJsonParser private extends Parser[Array[Byte]] {
  def input                                      = fail()
  def valueIndex                                 = fail()
  def pull(receiver: Receiver)                   = fail()
  def padByte()                                  = fail()
  def padDoubleByte(remaining: Int)              = fail()
  def padQuadByte(remaining: Int)                = fail()
  def padOctaByte(remaining: Int)                = fail()
  def padBytes(rest: Array[Byte], missing: Long) = fail()
  def release()                                  = fail()
  private def fail()                             = throw new UnsupportedOperationException
}
