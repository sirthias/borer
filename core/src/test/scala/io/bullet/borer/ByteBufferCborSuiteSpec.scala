/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.nio.ByteBuffer

class ByteBufferCborSuiteSpec extends AbstractCborSuiteSpec:

  def encode[T: Encoder](value: T): String =
    val byteBuffer = Cbor.encode(value).withConfig(Cbor.EncodingConfig(bufferSize = 8)).toByteBuffer
    toHexString(ByteAccess.ForByteBuffer.toByteArray(byteBuffer))

  def decode[T: Decoder](encoded: String): T =
    Cbor.decode(ByteBuffer.wrap(hexBytes(encoded))).to[T].value
