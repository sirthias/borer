/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import io.bullet.borer._
import _root_.scodec.bits.ByteVector
import scodec._

trait AbstractByteVectorSpec extends BorerSpec {

  override def encode[T: Encoder](value: T): String =
    toHexString(Cbor.encode(value).to[ByteVector].bytes.toArray)

  override def decode[T: Decoder](encoded: String): T =
    Cbor.decode(ByteVector(hexBytes(encoded))).to[T].value

}
