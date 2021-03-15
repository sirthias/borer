/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

/**
  * Abstraction for the "undefined" value in CBOR-speak.
  */
case object Undefined {

  implicit val codec = Codec[Undefined.type](
    encoder = (w, _) => w.writeUndefined(),
    decoder = { r =>
      r.readUndefined()
      Undefined
    }
  )
}
