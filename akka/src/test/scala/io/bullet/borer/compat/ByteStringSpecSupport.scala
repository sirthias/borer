/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import _root_.akka.util.ByteString
import io.bullet.borer.core.{BorerSpec, Input}
import akka._

trait ByteStringSpecSupport { this: BorerSpec[ByteString] ⇒
  val byteAccess                          = ByteStringByteAccess
  def newInput(bytes: Array[Byte]): Input = ByteString(bytes)
  def outResultByteAccess                 = byteAccess
}
