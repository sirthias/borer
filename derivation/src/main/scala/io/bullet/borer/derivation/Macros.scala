/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import scala.reflect.macros.blackbox

object Macros {

  def deriveCodec[T: c.WeakTypeTag](c: blackbox.Context): c.universe.Tree = {
    import c.universe._
    q"_root_.io.bullet.borer.core.Codec(deriveEncoder[${weakTypeOf[T]}], deriveDecoder[${weakTypeOf[T]}])"
  }
}
