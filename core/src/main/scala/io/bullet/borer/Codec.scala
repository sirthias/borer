/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import scala.annotation.threadUnsafe
import scala.deriving.Mirror

/**
 * A simple encapsulation of an [[Encoder]] and [[Decoder]] for the same type, as one entity.
 *
 * Sometimes it's easier to supply just a single given for a type, rather than two.
 * As an alternative to writing a separate [[Encoder]] and [[Decoder]] for type [[A]]
 * you can also write a [[Codec]] for [[A]].
 * ([[Encoder]] and [[Decoder]] can be implicitly "unpacked" from a codec.)
 *
 * However, in order to not hinder composability Codecs should only ever be _supplied_, never consumed.
 * So, if you write an encoder, decoder or codec for a generic type, which itself requires implicitly
 * available encoders and/or decoders for certain type parameters (like `Encoder.forOption`, for example)
 * then you should never require implicitly available Codecs, but rather Encoders and Decoders separately.
 */
case class Codec[A](encoder: Encoder[A], decoder: Decoder[A]):
  private given Encoder[A] = encoder
  private given Decoder[A] = decoder

  def bimap[B](f: B => A, g: A => B): Codec[B] =
    Codec.bimap(f, g)

  inline def bimapOption[B: Mirror.Of](f: B => A, g: A => Option[B]): Codec[B] =
    Codec.bimapOption(f, g)

  def bimapEither[E: Decoder.DecodingError, B](f: B => A, g: A => Either[E, B]): Codec[B] =
    Codec.bimapEither(f, g)

  def withEncoder(encoder: Encoder[A]): Codec[A] = copy(encoder = encoder)
  def withDecoder(decoder: Decoder[A]): Codec[A] = copy(decoder = decoder)

object Codec:

  /**
   * Same as `apply` but with the parameter list marked as `given`.
   */
  def of[T](using encoder: Encoder[T], decoder: Decoder[T]): Codec[T] =
    Codec(encoder, decoder)

  extension [T](underlying: Codec[T])
    /**
     * Wraps a [[Codec]] definition with lazy initialization.
     */
    def recursive: Codec[T] = Codec(underlying.encoder.recursive, underlying.decoder.recursive)

  /**
   * Convenience constructor.
   */
  inline def forProduct[T <: Product: Mirror.ProductOf]: Codec[T] =
    Codec(Encoder.forProduct[T], Decoder.forProduct[T])

  /**
   * Constructs a `Codec[B]` from an `Encoder[A]`, a `Decoder[A]` and two functions.
   */
  def bimap[A, B](f: B => A, g: A => B)(using ea: Encoder[A], da: Decoder[A]): Codec[B] =
    Codec(ea.contramap(f), da.map(g))

  /**
   * Constructs a `Codec[B]` from an `Encoder[A]`, a `Decoder[A]` and two functions.
   */
  inline def bimapOption[A, B: Mirror.Of](f: B => A, g: A => Option[B])(
      using ea: Encoder[A],
      da: Decoder[A]): Codec[B] =
    Codec(ea.contramap(f), da.mapOption(g))

  /**
   * Constructs a `Codec[B]` from an `Encoder[A]`, a `Decoder[A]` and two functions.
   */
  def bimapEither[A, E: Decoder.DecodingError, B](f: B => A, g: A => Either[E, B])(
      using ea: Encoder[A],
      da: Decoder[A]): Codec[B] =
    Codec(ea.contramap(f), da.mapEither(g))

  /**
   * Creates a "unified" [[Codec]] from two codecs that each target only a single data format.
   */
  def targetSpecific[T](cbor: Codec[T], json: Codec[T]): Codec[T] =
    Codec(Encoder.targetSpecific(cbor.encoder, json.encoder), Decoder.targetSpecific(cbor.decoder, json.decoder))

  /**
   * The default [[Codec]] for [[Either]] is not automatically in scope,
   * because there is no clear "standard" way of encoding instances of [[Either]].
   */
  object ForEither:

    given default[A: Encoder: Decoder, B: Encoder: Decoder]: Codec[Either[A, B]] =
      Codec(Encoder.ForEither.default, Decoder.ForEither.default)

  /**
   * Helper type serving only as the target of a `derives Codec.All` clause.
   * The `borer-derivation` module can then provide the respective `derived` method on the companion object.
   */
  case class All[A] private[borer] (delegate: Codec[A])
