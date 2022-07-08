/*
 * Copyright (c) 2019-2022 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

trait ByteArrayCborSpec extends AbstractBorerSpec:

  def encode[T: Encoder](value: T): String = toHexString(Cbor.encode(value).toByteArray)

  def decode[T: Decoder](encoded: String): T = Cbor.decode(hexBytes(encoded)).to[T].value
