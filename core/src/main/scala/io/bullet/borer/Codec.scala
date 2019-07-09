/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

/**
  * A simple encapsulation of an [[Encoder]] and [[Decoder]] for the same type, as one entity.
  *
  * Sometimes it's easier to supply just a single implicit for a type, rather than two.
  * As an alternative to writing a separate [[Encoder]] and [[Decoder]] for type [[A]]
  * you can also write a [[Codec]] for [[A]].
  * ([[Encoder]] and [[Decoder]] can be implicitly "unpacked" from a codec.)
  *
  * However, in order to not hinder composability Codecs should only ever be _supplied_, never consumed.
  * So, if you write an encoder, decoder or codec for a generic type, which itself requires implicitly
  * available encoders and/or decoders for certain type parameters (like `Encoder.forOption`, for example)
  * then you should never require implicitly available Codecs, but rather Encoders and Decoders separately.
  */
final case class Codec[A](encoder: Encoder[A], decoder: Decoder[A]) {

  @inline def bimap[B](f: B => A, g: A => B): Codec[B] = Codec.bimap(f, g)(encoder, decoder)
}

object Codec {

  /**
    * Wraps implicitly available [[Encoder]] and [[Decoder]] instances for [[T]] in a [[Codec]].
    */
  @inline def implicitly[T: Encoder: Decoder]: Codec[T] =
    Codec(Predef.implicitly[Encoder[T]], Predef.implicitly[Decoder[T]])

  /**
    * Constructs a `Codec[B]` from an `Encoder[A]`, a `Decoder[A]` and two functions.
    */
  @inline def bimap[A, B](f: B => A, g: A => B)(ea: Encoder[A], da: Decoder[A]): Codec[B] =
    Codec(ea contramap f, da map g)
}
