/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.*
import scala.quoted.*

/**
 * Same as [[MapBasedCodecs]], but with "compact", i.e. unwrapped, encodings for unary case classes.
 */
object CompactMapBasedCodecs extends DerivationApi {

  /**
   * Macro that creates an [[Encoder]] for [[T]] provided that
   * - [[T]] is a `case class`, `enum`, `sealed abstract class` or `sealed trait`
   * - [[Encoder]] instances for all members of [[T]] (if [[T]] is a `case class`)
   *   or all sub-types of [[T]] (if [[T]] is an ADT) are implicitly available
   *
   * Case classes are converted into a map of values, one key-value pair for each member.
   * The key for each member is a `String` holding the member's name.
   * This can be customized with the [[key]] annotation.
   *
   * NOTE: If `T` is unary (i.e. only has a single member) then the member value is written in an unwrapped form,
   * i.e. without the map container.
   */
  inline def deriveEncoder[T](using inline config: DerivationConfig): Encoder[T] = ${ Macros.encoder[T]('config) }

  /**
   * Macro that creates an [[Encoder]] for [[T]] and all direct and indirect sub-types of [[T]],
   * which are concrete, i.e. not abstract.
   * [[T]] must be an `enum`, `sealed abstract class` or `sealed trait`.
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
  inline def deriveAllEncoders[T](using inline config: DerivationConfig): Encoder[T] = ${ Macros.allEncoders[T]('config) }

  /**
   * Macro that creates a [[Decoder]] for [[T]] provided that
   * - [[T]] is a `case class`, `enum`, `sealed abstract class` or `sealed trait`
   * - [[Decoder]] instances for all members of [[T]] (if [[T]] is a `case class`)
   *   or all sub-types of [[T]] (if [[T]] is an ADT) are implicitly available
   *
   * Case classes are created from a map of values, one key-value pair for each member.
   * The key for each member is a `String` holding the member's name.
   * This can be customized with the [[key]] annotation.
   *
   * NOTE: If `T` is unary (i.e. only has a single member) then the member value is written in an unwrapped form,
   * i.e. without the map container.
   */
  inline def deriveDecoder[T](using inline config: DerivationConfig): Decoder[T] = ${ Macros.decoder[T]('config) }

  /**
   * Macro that creates a [[Decoder]] for [[T]] and all direct and indirect sub-types of [[T]],
   * which are concrete, i.e. not abstract.
   * [[T]] must be an `enum`, `sealed abstract class` or `sealed trait`.
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
  inline def deriveAllDecoders[T](using inline config: DerivationConfig): Decoder[T] = ${ Macros.allDecoders[T]('config) }

  /**
   * Macro that creates an [[Encoder]] and [[Decoder]] pair for [[T]].
   * Convenience shortcut for `Codec(deriveEncoder[T], deriveDecoder[T])`.
   */
  inline def deriveCodec[T](using inline config: DerivationConfig): Codec[T] =
    Codec(deriveEncoder[T](using config), deriveDecoder[T](using config))

  /**
   * Macro that creates an [[Encoder]] and [[Decoder]] pair for [[T]] and all direct and indirect sub-types of [[T]].
   * Convenience shortcut for `Codec(deriveAllEncoders[T], deriveAllDecoders[T])`.
   */
  inline def deriveAllCodecs[T](using inline config: DerivationConfig): Codec[T] =
    Codec(deriveAllEncoders[T](using config), deriveAllDecoders[T](using config))

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private[derivation] object Macros:

    def encoder[T: Type](configExpr: Expr[DerivationConfig])(using quotes: Quotes): Expr[Encoder[T]] =
      if (isUnaryCaseClass[T]) ArrayBasedCodecs.Macros.encoder[T](configExpr)
      else MapBasedCodecs.Macros.encoder[T](configExpr)

    def decoder[T: Type](configExpr: Expr[DerivationConfig])(using quotes: Quotes): Expr[Decoder[T]] =
      if (isUnaryCaseClass[T]) ArrayBasedCodecs.Macros.decoder[T](configExpr)
      else MapBasedCodecs.Macros.decoder[T](configExpr)

    def allEncoders[T: Type](configExpr: Expr[DerivationConfig])(using Quotes): Expr[Encoder[T]] =
      Derive.deriveAll[Encoder, T]("allEncoders", "deriveEncoder", configExpr) {
        new Derive.MacroCall[Encoder] {
          def apply[A: Type](using Quotes) = '{ deriveEncoder[A](using $configExpr) }
        }
      }

    def allDecoders[T: Type](configExpr: Expr[DerivationConfig])(using Quotes): Expr[Decoder[T]] =
      Derive.deriveAll[Decoder, T]("allDecoders", "deriveDecoder", configExpr) {
        new Derive.MacroCall[Decoder] {
          def apply[A: Type](using Quotes) = '{ deriveDecoder[A](using $configExpr) }
        }
      }

    private def isUnaryCaseClass[T: Type](using quotes: Quotes): Boolean =
      import quotes.reflect.*
      val tpe = TypeRepr.of[T].dealias.widen
      tpe.typeSymbol.flags.is(Flags.Case) && tpe.classSymbol.exists(_.caseFields.lengthCompare(1) == 0)

  end Macros
}
