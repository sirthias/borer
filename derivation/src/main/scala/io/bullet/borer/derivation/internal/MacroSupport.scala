/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation.internal

import io.bullet.borer.{Decoder, Encoder}

import scala.reflect.macros.blackbox

private[derivation] object MacroSupport {

  sealed trait Key extends Product {
    def value: Any
  }

  object Key {
    final case class String(value: java.lang.String) extends Key
    final case class Long(value: scala.Long)         extends Key
  }

  val EncoderPlaceholder = Encoder[Any]((_, _) => sys.error("Internal Error: Unresolved Encoder Placeholder"))
  val DecoderPlaceholder = Decoder[Any](_ => sys.error("Internal Error: Unresolved Decoder Placeholder"))

  val KeyPairOrdering: Ordering[Object] = {
    Ordering.fromLessThan[(Key, _)] {
      case ((Key.Long(x), _), (Key.Long(y), _))     => x < y
      case ((Key.String(x), _), (Key.String(y), _)) => x < y
      case ((x, _), _)                              => x.isInstanceOf[Key.Long] // we sort LongKeys before StringKeys
    }
  }.asInstanceOf[Ordering[Object]]

  def codecMacro[T: c.WeakTypeTag](c: blackbox.Context)(objectName: String): c.Tree = {
    import c.universe._

    val tpe      = weakTypeOf[T]
    val borerPkg = c.mirror.staticPackage("io.bullet.borer")
    val prefix   = q"$borerPkg.derivation.${TermName(objectName)}"
    q"$borerPkg.Codec($prefix.deriveEncoder[$tpe], $prefix.deriveDecoder[$tpe])"
  }
}
