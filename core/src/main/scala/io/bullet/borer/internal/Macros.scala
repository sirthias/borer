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
    val tpe     = weakTypeOf[T]
    val unapply = companionMethod(c)(tpe, "unapply")
    val params  = tpe.dealias.typeArgs
    q"""_root_.io.bullet.borer.Encoder.from($unapply[..$params] _)"""
  }

  def decoderForCaseClass[T: c.WeakTypeTag](c: blackbox.Context): c.universe.Tree = {
    import c.universe._
    val tpe            = weakTypeOf[T]
    val apply          = companionMethod(c)(tpe, "apply")
    val fields         = tpe.decls.collectFirst { case m: MethodSymbol if m.isPrimaryConstructor => m }.get.paramLists.head
    val names          = fields.map(_.asTerm.name)
    val namesWithTypes = names.map(name => q"$name : ${tpe.decl(name).typeSignature}")
    val params         = tpe.dealias.typeArgs
    q"""_root_.io.bullet.borer.Decoder.from((..$namesWithTypes) => $apply[..$params](..$names))"""
  }

  def codecForCaseClass[T: c.WeakTypeTag](c: blackbox.Context): c.universe.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]
    q"""_root_.io.bullet.borer.Codec[$tpe](
          _root_.io.bullet.borer.Encoder.forCaseClass[$tpe],
          _root_.io.bullet.borer.Decoder.forCaseClass[$tpe])"""
  }

  private def companionMethod(c: blackbox.Context)(tpe: c.universe.Type, method: String): c.universe.Tree = {
    import c.universe._
    val typeSymbol = tpe.typeSymbol
    val meth       = TermName(method)
    val companion  = typeSymbol.companion
    if (companion == NoSymbol) {
      if (typeSymbol.isClass && typeSymbol.asClass.isCaseClass) {
        q"${TermName(typeSymbol.name.decodedName.toString)}.$meth"
      } else c.abort(c.enclosingPosition, s"`$tpe` is not a case class")
    } else q"$companion.$meth"
  }

}
