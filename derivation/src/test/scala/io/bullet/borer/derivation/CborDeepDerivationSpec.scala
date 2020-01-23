/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer._
import utest._

object CborDeepDerivationSpec extends AbstractBorerSpec {

  def encode[T: Encoder](value: T): String =
    toHexString(Cbor.encode(value).withConfig(Cbor.EncodingConfig(bufferSize = 19)).toByteArray)
  def decode[T: Decoder](encoded: String): T = Cbor.decode(hexBytes(encoded)).to[T].value

  val tests = Tests {

    "mixed id types" - {
      sealed trait Ax
      @key(2) case object B extends Ax
      case object C         extends Ax
      case object D         extends Ax
      @key(1) case object E extends Ax
      @key(3) case object F extends Ax

      implicit val codec = MapBasedCodecs.deriveAllCodecs[Ax]

      roundTrip("a102a0", B: Ax)
      roundTrip("a16143a0", C: Ax)
      roundTrip("a16144a0", D: Ax)
      roundTrip("a101a0", E: Ax)
      roundTrip("a103a0", F: Ax)
    }
  }
}
