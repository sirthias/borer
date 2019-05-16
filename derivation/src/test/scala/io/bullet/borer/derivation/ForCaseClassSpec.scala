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

object ForCaseClassSpec extends AbstractBorerSpec {

  def encode[T: Encoder](value: T): String   = toHexString(Cbor.encode(value).toByteArray)
  def decode[T: Decoder](encoded: String): T = Cbor.decode(hexBytes(encoded)).to[T].value

  val tests = Tests {

    "Single-Member Case Class with custom apply" - {
      case class Box(id: String)
      object Box {
        def apply(): Box = new Box("default")
      }
      implicit val boxCodec: Codec[Box] = Codec.forCaseClass[Box]
      roundTrip("63616263", Box("abc"))
    }

  }
}
