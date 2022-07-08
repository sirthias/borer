/*
 * Copyright (c) 2019-2022 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.*
import scala.deriving.*
import scala.compiletime.*
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
  inline def deriveEncoder[T]: Encoder[T] = ${ Macros.encoder[T] }

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
  inline def deriveAllEncoders[T]: Encoder[T] = ${ Macros.allEncoders[T] }

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
  inline def deriveDecoder[T]: Decoder[T] = ${ Macros.decoder[T] }

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
  inline def deriveAllDecoders[T]: Decoder[T] = ${ Macros.allDecoders[T] }

  /**
   * Macro that creates an [[Encoder]] and [[Decoder]] pair for [[T]].
   * Convenience shortcut for `Codec(deriveEncoder[T], deriveDecoder[T])`.
   */
  inline def deriveCodec[T]: Codec[T] = Codec(deriveEncoder[T], deriveDecoder[T])

  /**
   * Macro that creates an [[Encoder]] and [[Decoder]] pair for [[T]] and all direct and indirect sub-types of [[T]].
   * Convenience shortcut for `Codec(deriveAllEncoders[T], deriveAllDecoders[T])`.
   */
  inline def deriveAllCodecs[T]: Codec[T] = Codec(deriveAllEncoders[T], deriveAllDecoders[T])

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private[derivation] object Macros:

    def encoder[T: Type](using quotes: Quotes): Expr[Encoder[T]] =
      if (isUnaryCaseClass[T]) ArrayBasedCodecs.Macros.encoder[T]
      else MapBasedCodecs.Macros.encoder[T]

    def decoder[T: Type](using quotes: Quotes): Expr[Decoder[T]] =
      if (isUnaryCaseClass[T]) ArrayBasedCodecs.Macros.decoder[T]
      else MapBasedCodecs.Macros.decoder[T]

    def allEncoders[T: Type](using Quotes): Expr[Encoder[T]] =
      Derive.deriveAll[Encoder, T]("allEncoders", "deriveEncoder") {
        new Derive.MacroCall[Encoder] {
          def apply[A: Type](using Quotes) = '{ deriveEncoder[A] }
        }
      }

    def allDecoders[T: Type](using Quotes): Expr[Decoder[T]] =
      Derive.deriveAll[Decoder, T]("allDecoders", "deriveDecoder") {
        new Derive.MacroCall[Decoder] {
          def apply[A: Type](using Quotes) = '{ deriveDecoder[A] }
        }
      }

    private def isUnaryCaseClass[T: Type](using quotes: Quotes): Boolean =
      import quotes.reflect.*
      val tpe = TypeRepr.of[T].dealias.widen
      tpe.typeSymbol.flags.is(Flags.Case) && tpe.classSymbol.exists(_.caseFields.lengthCompare(1) == 0)

  end Macros
}
