/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import scala.reflect.macros.blackbox

object Macros {

  def encoderForCaseClass[T: c.WeakTypeTag](c: blackbox.Context): c.universe.Tree = {
    import c.universe._
    val tpe       = weakTypeOf[T]
    val companion = tpe.typeSymbol.companion
    if (companion == NoSymbol) c.abort(c.enclosingPosition, s"`$tpe` is not a case class")
    val params = tpe.dealias.typeArgs
    q"""_root_.io.bullet.borer.Encoder.from($companion.unapply[..$params] _)"""
  }

  def decoderForCaseClass[T: c.WeakTypeTag](c: blackbox.Context): c.universe.Tree = {
    import c.universe._
    val tpe       = weakTypeOf[T]
    val companion = tpe.typeSymbol.companion
    if (companion == NoSymbol) c.abort(c.enclosingPosition, s"`$tpe` is not a case class")
    val params = tpe.dealias.typeArgs
    q"""_root_.io.bullet.borer.Decoder.from($companion.apply[..$params] _)"""
  }

  def codecForCaseClass[T: c.WeakTypeTag](c: blackbox.Context): c.universe.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]
    q"""_root_.io.bullet.borer.Codec[$tpe](
        _root_.io.bullet.borer.Encoder.forCaseClass[$tpe],
        _root_.io.bullet.borer.Decoder.forCaseClass[$tpe])"""
  }
}
