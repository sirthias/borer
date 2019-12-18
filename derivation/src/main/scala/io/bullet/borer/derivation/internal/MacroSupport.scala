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

  def codecMacro[T: c.WeakTypeTag](c: blackbox.Context)(objectName: String, de: String, dd: String): c.Tree = {
    import c.universe._
    val tpe      = weakTypeOf[T]
    val borerPkg = c.mirror.staticPackage("_root_.io.bullet.borer")
    val prefix   = q"$borerPkg.derivation.${TermName(objectName)}"
    q"$borerPkg.Codec($prefix.${TermName(de)}[$tpe], $prefix.${TermName(dd)}[$tpe])"
  }

  def deriveAll[T: ctx.WeakTypeTag](
      ctx: blackbox.Context)(typeClass: String, objectName: String, macroName: String, altMacroName: String): ctx.Tree =
    DeriveWith[T](ctx) {
      new CodecDeriver[ctx.type](ctx) {
        import c.universe._

        def deriveForCaseObject(tpe: Type, module: ModuleSymbol) =
          c.abort(
            c.enclosingPosition,
            s"The `$macroName` macro can only be used on sealed traits or sealed abstract " +
              s"classes, not on case objects. Use `$altMacroName` instead!")

        def deriveForCaseClass(
            tpe: Type,
            companion: ModuleSymbol,
            params: List[CaseParam],
            annotationTrees: List[Tree],
            constructorIsPrivate: Boolean) =
          c.abort(
            c.enclosingPosition,
            s"The `$macroName` macro can only be used on sealed traits or sealed abstract " +
              s"classes, not on case classes. Use `$altMacroName` instead!")

        def deriveForSealedTrait(tpe: Type, subTypes: List[SubType]) = {
          val altMacro = q"$borerPkg.derivation.${TermName(objectName)}.${TermName(altMacroName)}"
          val tc =
            typeClass match {
              case "Encoder" => encoderType
              case "Decoder" => decoderType
            }

          def implicitSubTypeVals(subTypes: List[SubType]): List[Tree] =
            subTypes.flatMap { st =>
              if (inferImplicit(tc, st.tpe).isEmpty) {
                if (st.isAbstract) implicitSubTypeVals(st.subs)
                else Some(q"implicit val ${TermName(c.freshName())} = $altMacro[${st.tpe}]")
              } else None
            }

          q"""{
            ..${implicitSubTypeVals(subTypes)}
            $altMacro[$tpe]
          }"""
        }
      }
    }
}
