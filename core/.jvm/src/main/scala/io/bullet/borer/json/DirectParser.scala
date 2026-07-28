/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.json

import io.bullet.borer.Json
import io.bullet.borer.internal.DirectByteArrayAccess

private[borer] object DirectParser:

  def apply(inputValue: Any, config: Json.DecodingConfig): DirectJsonParser =
    inputValue match
      case input: Array[Byte] if config.allowDirectParsing =>
        new DirectJsonParser(new io.bullet.borer.Input.FromByteArray(input), config)
      case _ => null
