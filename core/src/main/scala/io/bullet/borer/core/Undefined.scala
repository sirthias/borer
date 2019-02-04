/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

/**
  * Abstraction for the "undefined" value in CBOR-speak.
  */
case object Undefined {

  implicit val codec = Codec
    .of[Undefined.type]
    .from(
      encode = (w, _) ⇒ w.writeUndefined(),
      decode = { r ⇒
        r.readUndefined()
        Undefined
      }
    )
}
