/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.*

trait DerivationApi {

  inline def deriveEncoder[T]: Encoder[T]

  inline def deriveAllEncoders[T]: Encoder[T]

  inline def deriveDecoder[T]: Decoder[T]

  inline def deriveAllDecoders[T]: Decoder[T]

  inline def deriveCodec[T]: Codec[T]

  inline def deriveAllCodecs[T]: Codec[T]

}
