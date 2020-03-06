/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import _root_.scodec.bits.ByteVector
import io.bullet.borer._

object ScodecCborSuiteSpec extends AbstractCborSuiteSpec {
  import scodec._

  def encode[T: Encoder](value: T): String   = toHexString(Cbor.encode(value).to[ByteVector].result.toArray)
  def decode[T: Decoder](encoded: String): T = Cbor.decode(ByteVector(hexBytes(encoded))).to[T].value
}
