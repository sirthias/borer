/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.*

trait DerivationApi {

  inline def deriveEncoder[T]: Encoder[T]

  inline def deriveAllEncoders[T]: Encoder[T]

  inline def deriveDecoder[T]: Decoder[T]

  inline def deriveAllDecoders[T]: Decoder[T]

  inline def deriveCodec[T]: Codec[T]

  inline def deriveAllCodecs[T]: Codec[T]

  /**
   * This enables [[Encoder]] derivation via the `derives` clauses, e.g.
   * {{{
   * import io.bullet.borer.derivation.MapBasedCodecs.*
   *
   * case class Foo(x: Int) derives Encoder
   * }}}
   */
  extension (c: Encoder.type) inline def derived[A]: Encoder[A] = deriveEncoder[A]

  /**
   * This enables [[Decoder]] derivation via the `derives` clauses, e.g.
   * {{{
   * import io.bullet.borer.derivation.MapBasedCodecs.*
   *
   * case class Foo(x: Int) derives Decoder
   * }}}
   */
  extension (c: Decoder.type) inline def derived[A]: Decoder[A] = deriveDecoder[A]

  /**
   * This enables [[Codec]] derivation via the `derives` clauses, e.g.
   * {{{
   * import io.bullet.borer.derivation.MapBasedCodecs.*
   *
   * case class Foo(x: Int) derives Codec
   * }}}
   */
  extension (c: Codec.type) inline def derived[A]: Codec[A] = deriveCodec[A]

  /**
   * This enables [[Encoder]] derivation via the `derives` clause, e.g.
   * {{{
   * import io.bullet.borer.derivation.MapBasedCodecs.*
   *
   * enum Bar derives Encoder.All:
   *    ...
   * }}}
   */
  extension (c: Encoder.All.type) inline def derived[A]: Encoder.All[A] = Encoder.All(deriveAllEncoders[A])

  /**
   * This enables [[Decoder]] derivation via the `derives` clause, e.g.
   * {{{
   * import io.bullet.borer.derivation.MapBasedCodecs.*
   *
   * enum Bar derives Decoder.All:
   *    ...
   * }}}
   */
  extension (c: Decoder.All.type) inline def derived[A]: Decoder.All[A] = Decoder.All(deriveAllDecoders[A])

  /**
   * This enables [[Codec]] derivation via the `derives` clause, e.g.
   * {{{
   * import io.bullet.borer.derivation.MapBasedCodecs.*
   *
   * enum Bar derives Codec.All:
   *   ...
   * }}}
   */
  extension (c: Codec.All.type) inline def derived[A]: Codec.All[A] = Codec.All(deriveAllCodecs[A])

}
