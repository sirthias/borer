/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.nio.charset.StandardCharsets

object ByteArrayJsonSpec extends AbstractJsonSpec {

  def encode[T: Encoder](value: T): String = Json.encode(value).toUtf8String

  def decode[T: Decoder](encoded: String): T =
    Json
      .decode(encoded getBytes StandardCharsets.UTF_8)
      .withConfig(Json.DecodingConfig.default.copy(maxNumberAbsExponent = 300, initialCharbufferSize = 8))
      .to[T]
      .value
}
