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

  def deriveCodecImpl[T: c.WeakTypeTag](c: blackbox.Context): c.universe.Tree = {
    import c.universe._
    q"_root_.io.bullet.borer.core.Codec(deriveEncoder[${weakTypeOf[T]}], deriveDecoder[${weakTypeOf[T]}])"
  }

  def deriveCaseClassCodecImpl[T: c.WeakTypeTag](c: blackbox.Context): c.universe.Tree = {
    import c.universe._
    val tpe       = weakTypeOf[T]
    val companion = tpe.typeSymbol.companion
    if (companion == NoSymbol) c.abort(c.enclosingPosition, s"`$tpe` is not a case class")
    q"""_root_.io.bullet.borer.core.Codec[$tpe](
        _root_.io.bullet.borer.core.Encoder.from($companion.unapply _),
        _root_.io.bullet.borer.core.Decoder.from($companion.apply _))"""
  }
}
