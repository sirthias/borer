/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer._
import io.bullet.borer.derivation.internal._

import scala.reflect.macros.blackbox

object ArrayBasedCodecs {

  def deriveEncoder[T]: Encoder[T] = macro Macros.encoder[T]

  def deriveDecoder[T]: Decoder[T] = macro Macros.decoder[T]

  def deriveCodec[T]: Codec[T] = macro Macros.codec[T]

  private object Macros {
    import MacroSupport._

    def encoder[T: ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Tree = DeriveWith[T](ctx) {
      new CodecDeriver[ctx.type](ctx) {
        import c.universe._

        def deriveForCaseClass(
            tpe: Type,
            companion: ModuleSymbol,
            params: List[CaseParam],
            annotationTrees: List[Tree],
            constructorIsPrivate: Boolean) = {

          val arity     = params.size
          val writeOpen = if (arity == 1) q"w" else q"w.writeArrayOpen($arity)"
          val writeOpenAndFields = params.foldLeft(writeOpen) { (acc, field) =>
            q"$acc.${TermName(s"write${field.basicTypeNameOrEmpty}")}(x.${field.name})"
          }
          val writeOpenFieldsAndClose = if (arity == 1) writeOpenAndFields else q"$writeOpenAndFields.writeArrayClose()"
          q"""_root_.io.bullet.borer.Encoder((w, x) => $writeOpenFieldsAndClose)"""
        }

        def deriveForSealedTrait(tpe: Type, subTypes: List[SubType]) = {
          val cases = adtSubtypeWritingCases(tpe, subTypes)
          q"""_root_.io.bullet.borer.Encoder { (w, value) =>
                w.writeArrayOpen(2)
                value match { case ..$cases }
                w.writeArrayClose()
              }"""
        }
      }
    }

    def decoder[T: ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Tree = DeriveWith[T](ctx) {
      new CodecDeriver[ctx.type](ctx) {
        import c.universe._

        def deriveForCaseClass(
            tpe: Type,
            companion: ModuleSymbol,
            params: List[CaseParam],
            annotationTrees: List[Tree],
            constructorIsPrivate: Boolean) = {

          if (constructorIsPrivate)
            error(s"Cannot derive Decoder[$tpe] because the primary constructor of `$tpe` is private")

          val readFields = params.map { p =>
            val paramType = p.paramType.tpe
            if (isBasicType(paramType)) q"r.${TermName(s"read$paramType")}()" else q"r.read[$paramType]()"
          }
          val arity               = params.size
          val readObject          = q"$companion.apply(..$readFields)"
          def expected(s: String) = s"$s for decoding an instance of type `$tpe`"
          val readObjectWithWrapping =
            arity match {
              case 0 =>
                q"""if (r.tryReadArrayHeader(0) || r.tryReadArrayStart() && r.tryReadBreak()) $readObject
                    else r.unexpectedDataItem(${expected(s"Empty array")})"""
              case 1 => readObject
              case x =>
                q"""def readObject() = $readObject
                    if (r.tryReadArrayStart()) {
                      val result = readObject()
                      if (r.tryReadBreak()) result
                      else r.unexpectedDataItem(${expected(s"Array with $x elements")}, "at least one extra element")
                    } else if (r.tryReadArrayHeader($x)) readObject()
                    else r.unexpectedDataItem(${expected(s"Array Start or Array Header ($x)")})"""
            }
          q"_root_.io.bullet.borer.Decoder(r => $readObjectWithWrapping)"
        }

        def deriveForSealedTrait(tpe: Type, subTypes: List[SubType]) = {
          val typeIdsAndSubTypes: Array[(Key, SubType)] = getTypeIds(tpe, subTypes).zip(subTypes)
          java.util.Arrays.sort(typeIdsAndSubTypes.asInstanceOf[Array[Object]], KeyPairOrdering)

          def rec(start: Int, end: Int): Tree =
            if (start < end) {
              val mid           = (start + end) >> 1
              val (typeId, sub) = typeIdsAndSubTypes(mid)
              val cmp           = r("tryRead", typeId, "Compare")
              if (start < mid) {
                q"""val cmp = $cmp
                  if (cmp < 0) ${rec(start, mid)}
                  else if (cmp > 0) ${rec(mid + 1, end)}
                  else r.read[${sub.tpe}]()"""
              } else q"if ($cmp == 0) r.read[${sub.tpe}]() else fail()"
            } else q"fail()"

          val readTypeIdAndValue = rec(0, typeIdsAndSubTypes.length)

          q"""_root_.io.bullet.borer.Decoder { r =>
                def fail() = r.unexpectedDataItem(${s"type id key for subtype of `$tpe`"})
                r.readArrayClose(r.readArrayOpen(2), $readTypeIdAndValue)
              }"""
        }
      }
    }

    def codec[T: c.WeakTypeTag](c: blackbox.Context): c.Tree = codecMacro(c)("ArrayBasedCodecs")
  }
}
