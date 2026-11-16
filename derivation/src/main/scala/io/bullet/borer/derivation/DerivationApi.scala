/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.*

trait DerivationApi {

  inline def deriveEncoder[T](using inline config: DerivationConfig): Encoder[T]

  inline def deriveAllEncoders[T](using inline config: DerivationConfig): Encoder[T]

  inline def deriveDecoder[T](using inline config: DerivationConfig): Decoder[T]

  inline def deriveAllDecoders[T](using inline config: DerivationConfig): Decoder[T]

  inline def deriveCodec[T](using inline config: DerivationConfig): Codec[T]

  inline def deriveAllCodecs[T](using inline config: DerivationConfig): Codec[T]

  /**
   * This enables [[Encoder]] derivation via the `derives` clauses, e.g.
   * {{{
   * import io.bullet.borer.derivation.MapBasedCodecs.*
   *
   * case class Foo(x: Int) derives Encoder
   * }}}
   */
  extension (c: Encoder.type)
    inline def derived[A](using inline config: DerivationConfig): Encoder[A] = deriveEncoder[A](using config)

  /**
   * This enables [[Decoder]] derivation via the `derives` clauses, e.g.
   * {{{
   * import io.bullet.borer.derivation.MapBasedCodecs.*
   *
   * case class Foo(x: Int) derives Decoder
   * }}}
   */
  extension (c: Decoder.type)
    inline def derived[A](using inline config: DerivationConfig): Decoder[A] = deriveDecoder[A](using config)

  /**
   * This enables [[Codec]] derivation via the `derives` clauses, e.g.
   * {{{
   * import io.bullet.borer.derivation.MapBasedCodecs.*
   *
   * case class Foo(x: Int) derives Codec
   * }}}
   */
  extension (c: Codec.type)
    inline def derived[A](using inline config: DerivationConfig): Codec[A] = deriveCodec[A](using config)

  /**
   * This enables [[Encoder]] derivation via the `derives` clause, e.g.
   * {{{
   * import io.bullet.borer.derivation.MapBasedCodecs.*
   *
   * enum Bar derives Encoder.All:
   *    ...
   * }}}
   */
  extension (c: Encoder.All.type)
    inline def derived[A](using inline config: DerivationConfig): Encoder.All[A] =
      Encoder.All(deriveAllEncoders[A](using config))

  /**
   * This enables [[Decoder]] derivation via the `derives` clause, e.g.
   * {{{
   * import io.bullet.borer.derivation.MapBasedCodecs.*
   *
   * enum Bar derives Decoder.All:
   *    ...
   * }}}
   */
  extension (c: Decoder.All.type)
    inline def derived[A](using inline config: DerivationConfig): Decoder.All[A] =
      Decoder.All(deriveAllDecoders[A](using config))

  /**
   * This enables [[Codec]] derivation via the `derives` clause, e.g.
   * {{{
   * import io.bullet.borer.derivation.MapBasedCodecs.*
   *
   * enum Bar derives Codec.All:
   *   ...
   * }}}
   */
  extension (c: Codec.All.type)
    inline def derived[A](using inline config: DerivationConfig): Codec.All[A] =
      Codec.All(deriveAllCodecs[A](using config))

}
