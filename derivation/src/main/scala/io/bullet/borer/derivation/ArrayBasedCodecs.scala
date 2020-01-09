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

/**
  * Derivation macros for array-based encodings.
  */
object ArrayBasedCodecs {

  /**
    * Macro that creates an [[Encoder]] for [[T]] provided that
    * - [[T]] is a `case class`, `sealed abstract class` or `sealed trait`
    * - [[Encoder]] instances for all members of [[T]] (if [[T]] is a `case class`)
    *   or all sub-types of [[T]] (if [[T]] is an ADT) are implicitly available
    *
    * Case classes are converted into an array of values, one value for each member.
    *
    * NOTE: If `T` is unary (i.e. only has a single member) then the member value is written in an unwrapped form,
    * i.e. without the array container.
    */
  def deriveEncoder[T]: Encoder[T] = macro Macros.encoder[T]

  /**
    * Macro that creates an [[Encoder]] for [[T]] and all direct and indirect sub-types of [[T]],
    * which are concrete, i.e. not abstract.
    * [[T]] must be a `sealed abstract class` or `sealed trait`.
    *
    * It works by generating a code block such as this one:
    *
    * {{{
    *   implicit val a = deriveEncoder[A]     // one such line is generated for each concrete
    *   implicit val b = deriveEncoder[B]     // direct or indirect sub-type of T which doesn't
    *   implicit val c = deriveEncoder[C]     // already have an implicit Encoder available
    *   ...
    *   deriveEncoder[T]
    * }}}
    *
    * If an [[Encoder]] for a certain concrete sub-type `S <: T` is already implicitly available
    * at the macro call-site the respective line for the sub-type is **not** generated.
    *
    * If an [[Encoder]] for a certain abstract sub-type `S <: T` is already implicitly available
    * at the macro call-site the respective lines for **all** sub-types of `S` are **not** generated.
    *
    * This means that you can specify your own custom Encoders for concrete sub-types or whole branches
    * of the sub-type hierarchy and they will be properly picked up rather than create conflicts.
    */
  def deriveAllEncoders[T]: Encoder[T] = macro Macros.allEncoders[T]

  /**
    * Macro that creates a [[Decoder]] for [[T]] provided that
    * - [[T]] is a `case class`, `sealed abstract class` or `sealed trait`
    * - [[Decoder]] instances for all members of [[T]] (if [[T]] is a `case class`)
    *   or all sub-types of [[T]] (if [[T]] is an ADT) are implicitly available
    *
    * Case classes are created from an array of deserialized values, one value for each member.
    *
    * NOTE: If `T` is unary (i.e. only has a single member) then the member value is expected in an unwrapped form,
    * i.e. without the array container.
    */
  def deriveDecoder[T]: Decoder[T] = macro Macros.decoder[T]

  /**
    * Macro that creates a [[Decoder]] for [[T]] and all direct and indirect sub-types of [[T]],
    * which are concrete, i.e. not abstract.
    * [[T]] must be a `sealed abstract class` or `sealed trait`.
    *
    * It works by generating a code block such as this one:
    *
    * {{{
    *   implicit val a = deriveDecoder[A]     // one such line is generated for each concrete
    *   implicit val b = deriveDecoder[B]     // direct or indirect sub-type of T which doesn't
    *   implicit val c = deriveDecoder[C]     // already have an implicit Decoder available
    *   ...
    *   deriveDecoder[T]
    * }}}
    *
    * If a [[Decoder]] for a certain concrete sub-type `S <: T` is already implicitly available
    * at the macro call-site the respective line for the sub-type is **not** generated.
    *
    * If a [[Decoder]] for a certain abstract sub-type `S <: T` is already implicitly available
    * at the macro call-site the respective lines for **all** sub-types of `S` are **not** generated.
    *
    * This means that you can specify your own custom Decoders for concrete sub-types or whole branches
    * of the sub-type hierarchy and they will be properly picked up rather than create conflicts.
    */
  def deriveAllDecoders[T]: Decoder[T] = macro Macros.allDecoders[T]

  /**
    * Macro that creates an [[Encoder]] and [[Decoder]] pair for [[T]].
    * Convenience shortcut for `Codec(deriveEncoder[T], deriveDecoder[T])"`.
    */
  def deriveCodec[T]: Codec[T] = macro Macros.codec[T]

  /**
    * Macro that creates an [[Encoder]] and [[Decoder]] pair for [[T]] and all direct and indirect sub-types of [[T]].
    * Convenience shortcut for `Codec(deriveAllEncoders[T], deriveAllDecoders[T])"`.
    */
  def deriveAllCodecs[T]: Codec[T] = macro Macros.allCodecs[T]

  private object Macros {
    import MacroSupport._

    def encoder[T: ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Tree = DeriveWith[T](ctx) {
      new CodecDeriver[ctx.type](ctx) {
        import c.universe._

        def deriveForCaseObject(tpe: Type, module: ModuleSymbol) =
          q"$encoderCompanion[${module.typeSignature}]((w, _) => w.writeEmptyArray())"

        def deriveForCaseClass(
            tpe: Type,
            companion: ModuleSymbol,
            params: List[CaseParam],
            annotationTrees: List[Tree],
            constructorIsPrivate: Boolean) = {

          val arity     = params.size
          val writeOpen = if (arity == 1) q"w" else q"w.writeArrayOpen($arity)"
          val writeOpenAndFields = params.foldLeft(writeOpen) { (acc, field) =>
            val suffix =
              if (field.isBasicType && isDefinedOn(field.getImplicit(encoderType).get, encoderType)) {
                field.paramType.tpe.toString
              } else ""
            q"$acc.${TermName(s"write$suffix")}(x.${field.name})"
          }
          val writeOpenFieldsAndClose = if (arity == 1) writeOpenAndFields else q"$writeOpenAndFields.writeArrayClose()"
          q"""$encoderCompanion((w, x) => $writeOpenFieldsAndClose)"""
        }

        def deriveForSealedTrait(node: AdtTypeNode) = {
          val cases = adtSubtypeWritingCases(node)
          q"""$encoderCompanion { (w, value) =>
                w.writeArrayOpen(2)
                value match { case ..$cases }
                w.writeArrayClose()
              }"""
        }
      }
    }

    def allEncoders[T: c.WeakTypeTag](c: blackbox.Context): c.Tree =
      deriveAll(c)(isEncoder = true, "ArrayBasedCodecs", "deriveAllEncoders", "deriveEncoder")

    def decoder[T: ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Tree = DeriveWith[T](ctx) {
      new CodecDeriver[ctx.type](ctx) {
        import c.universe._

        def deriveForCaseObject(tpe: Type, module: ModuleSymbol) =
          q"$decoderCompanion(r => r.readArrayClose(r.readArrayOpen(0), $module))"

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
            if (isBasicType(paramType) && isDefinedOn(p.getImplicit(decoderType).get, decoderCompanion)) {
              q"r.${TermName(s"read$paramType")}()"
            } else q"r.read[$paramType]()"
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
          q"$decoderCompanion(r => $readObjectWithWrapping)"
        }

        def deriveForSealedTrait(node: AdtTypeNode) = {
          val typeIdsAndSubTypes = typeIdsAndFlattenedSubsSorted(node, decoderType)

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

          q"""$decoderCompanion { r =>
                def fail() = r.unexpectedDataItem(${s"type id key for subtype of `${node.tpe}`"})
                r.readArrayClose(r.readArrayOpen(2), $readTypeIdAndValue)
              }"""
        }
      }
    }

    def codec[T: c.WeakTypeTag](c: blackbox.Context): c.Tree =
      codecMacro(c)("ArrayBasedCodecs", "deriveEncoder", "deriveDecoder")

    def allDecoders[T: c.WeakTypeTag](c: blackbox.Context): c.Tree =
      deriveAll(c)(isEncoder = false, "ArrayBasedCodecs", "deriveAllDecoders", "deriveDecoder")

    def allCodecs[T: c.WeakTypeTag](c: blackbox.Context): c.Tree =
      codecMacro(c)("ArrayBasedCodecs", "deriveAllEncoders", "deriveAllDecoders")
  }
}
