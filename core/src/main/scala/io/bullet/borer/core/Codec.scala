/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.core

/**
  * A simple encapsulation of an [[Encoder]] and [[Decoder]] for the same type, as one entity.
  *
  * Sometimes it's easier to supply just a single implicit for a type, rather than two.
  * As an alternative to writing a separate [[Encoder]] and [[Decoder]] for type `T`
  * you can also write a [[Codec]] for `T`.
  * ([[Encoder]] and [[Decoder]] can be implicitly "unpacked" from a codec.)
  *
  * However, in order to not hinder composability Codecs should only ever be _supplied_,
  * never consumed. So, if you write an encoder, decoder or codec for a generic type,
  * which itself requires implicitly available encoders and/or decoders for certain type parameters
  * (like `Encoder.forOption`, for example) then you should never require implicitly available
  * Codecs, but rather Encoders and Decoders separately.
  */
final case class Codec[+EBytes, -DBytes, T](encoder: Encoder[EBytes, T], decoder: Decoder[DBytes, T])

object Codec {

  type Universal[T] = Codec[Nothing, Any, T]

  private[this] val builderSingleton = new Builder[Nothing, Any, Nothing]

  def of[T]: Builder[Nothing, Any, T] = builderSingleton.asInstanceOf[Builder[Nothing, Any, T]]

  final class Builder[EBytes, DBytes, T] private[Codec] {
    def from(encode: (Writer[EBytes], T) ⇒ Unit, decode: Reader[DBytes] ⇒ T): Codec[EBytes, DBytes, T] =
      Codec(Encoder(encode), Decoder(decode))

    def withBytes[Bytes]: Builder[Bytes, Bytes, T] = builderSingleton.asInstanceOf[Builder[Bytes, Bytes, T]]
  }
}
