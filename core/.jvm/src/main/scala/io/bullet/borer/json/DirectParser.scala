/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.json

import io.bullet.borer.Json
import io.bullet.borer.internal.Unsafe
import io.bullet.borer.internal.Unsafe.LittleEndianByteArrayAccess

private[borer] object DirectParser {

  def apply(inputValue: Any, config: Json.DecodingConfig): DirectJsonParser =
    inputValue match {
      case input: Array[Byte] if config.allowDirectParsing =>
        Unsafe.byteArrayAccess match {
          case baa: LittleEndianByteArrayAccess =>
            new DirectJsonParser(new io.bullet.borer.input.DirectFromByteArrayInput(input, baa), config)
          case _ => null
        }
      case _ => null
    }
}
