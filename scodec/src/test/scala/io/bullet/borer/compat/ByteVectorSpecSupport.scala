/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import io.bullet.borer.core.{BorerSpec, Input}
import _root_.scodec.bits.ByteVector
import scodec._

trait ByteVectorSpecSupport { this: BorerSpec[ByteVector] ⇒
  val byteAccess                          = ByteVectorByteAccess
  def newInput(bytes: Array[Byte]): Input = ByteVector(bytes)
  def outResultByteAccess                 = byteAccess
}
