/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import io.bullet.borer.core.AbstractRfcExamplesSpec
import _root_.akka.util.ByteString
import akka._

object AkkaSupportSpec extends AbstractRfcExamplesSpec[ByteString]("ByteString") {

  def newOutput = new ByteStringOutput

  def newInput(bytes: Array[Byte]) = ByteString(bytes)
}
