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
  * Same as [[MapBasedCodecs]], but with "compact", i.e. unwrapped, encodings for unary case classes.
  */
object CompactMapBasedCodecs {

  /**
    * Macro that creates an [[Encoder]] for [[T]] provided that
    * - [[T]] is a `case class`, `sealed abstract class` or `sealed trait`
    * - [[Encoder]] instances for all members of [[T]] (if [[T]] is a `case class`)
    *   or all sub-types of [[T]] (if [[T]] is an ADT) are implicitly available
    *
    * Case classes are converted into a map of values, one key-value pair for each member.
    * The key for each member is a `String` holding the member's name.
    * This can be customized with the [[key]] annotation.
    *
    * NOTE: If `T` is unary (i.e. only has a single member) then the member value is written in an unwrapped form,
    * * i.e. without the map container.
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
    * Case classes are created from a map of values, one key-value pair for each member.
    * The key for each member is a `String` holding the member's name.
    * This can be customized with the [[key]] annotation.
    *
    * NOTE: If `T` is unary (i.e. only has a single member) then the member value is expected in an unwrapped form,
    * * i.e. without the map container.
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

    def encoder[T: c.WeakTypeTag](c: blackbox.Context): c.Tree =
      forwardToArrayBasedOrMapBasedDependingOnArity[T](c)("deriveEncoder")

    def allEncoders[T: c.WeakTypeTag](c: blackbox.Context): c.Tree =
      deriveAll(c)("Encoder", "CompactMapBasedCodecs", "deriveAllEncoders", "deriveEncoder")

    def decoder[T: c.WeakTypeTag](c: blackbox.Context): c.Tree =
      forwardToArrayBasedOrMapBasedDependingOnArity[T](c)("deriveDecoder")

    def allDecoders[T: c.WeakTypeTag](c: blackbox.Context): c.Tree =
      deriveAll(c)("Decoder", "CompactMapBasedCodecs", "deriveAllDecoders", "deriveDecoder")

    def codec[T: c.WeakTypeTag](c: blackbox.Context): c.Tree =
      codecMacro(c)("CompactMapBasedCodecs", "deriveEncoder", "deriveDecoder")

    def allCodecs[T: c.WeakTypeTag](c: blackbox.Context): c.Tree =
      codecMacro(c)("CompactMapBasedCodecs", "deriveAllEncoders", "deriveAllDecoders")

    private def forwardToArrayBasedOrMapBasedDependingOnArity[T: c.WeakTypeTag](c: blackbox.Context)(
        macroName: String): c.Tree = {
      import c.universe._
      val tpe        = weakTypeOf[T].dealias
      val borerPkg   = c.mirror.staticPackage("_root_.io.bullet.borer")
      val iter       = tpe.decls.collect { case m: MethodSymbol if m.isCaseAccessor => m.asMethod }.iterator
      val isUnary    = iter.hasNext && { iter.next(); !iter.hasNext }
      val objectName = if (isUnary) "ArrayBasedCodecs" else "MapBasedCodecs"
      q"$borerPkg.derivation.${TermName(objectName)}.${TermName(macroName)}[$tpe]"
    }
  }
}
