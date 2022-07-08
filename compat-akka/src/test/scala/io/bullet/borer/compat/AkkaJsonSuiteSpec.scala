/*
 * Copyright (c) 2019-2022 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import java.nio.charset.StandardCharsets

import _root_.akka.util.ByteString
import io.bullet.borer._

class AkkaJsonSuiteSpec extends AbstractJsonSuiteSpec {
  import akka._

  def encode[T: Encoder](value: T): String =
    Json.encode(value).to[ByteString].result.utf8String

  def decode[T: Decoder](encoded: String): T =
    Json
      .decode(ByteString(encoded getBytes StandardCharsets.UTF_8))
      .withConfig(Json.DecodingConfig.default.copy(maxNumberAbsExponent = 300))
      .to[T]
      .value
}
