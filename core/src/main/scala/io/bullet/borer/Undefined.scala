/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

/**
 * Abstraction for the "undefined" value in CBOR-speak.
 */
case object Undefined:

  given Encoder[Undefined.type] = Encoder((w, _) => w.writeUndefined())
  given Decoder[Undefined.type] = Decoder { r =>
    r.readUndefined()
    Undefined
  }
