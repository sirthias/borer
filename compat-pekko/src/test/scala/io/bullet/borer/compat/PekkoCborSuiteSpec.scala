/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import _root_.org.apache.pekko.util.ByteString
import io.bullet.borer.*

class PekkoCborSuiteSpec extends AbstractCborSuiteSpec {
  import pekko.*

  def encode[T: Encoder](value: T): String   = toHexString(Cbor.encode(value).to[ByteString].result.toArray)
  def decode[T: Decoder](encoded: String): T = Cbor.decode(ByteString(hexBytes(encoded))).to[T].value
}
