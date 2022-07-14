/*
 * Copyright (c) 2019-2022 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.*

import java.nio.charset.StandardCharsets

enum Body {
  case Earth, Moon
  case Asteroid(mass: Double) extends Body
  case Sun                    extends Body
}

class EnumSpec extends AbstractBorerSpec {

  def encode[T: Encoder](value: T): String   = Json.encode(value).toUtf8String
  def decode[T: Decoder](encoded: String): T = Json.decode(encoded getBytes StandardCharsets.UTF_8).to[T].value

  test("Enum") {
    given Codec[Body] = MapBasedCodecs.deriveAllCodecs[Body]

    roundTrip("""{"Earth":0}""", Body.Earth: Body)
    roundTrip("""{"Moon":0}""", Body.Moon: Body)
    roundTrip("""{"Asteroid":{"mass":18.3}}""", Body.Asteroid(18.3): Body)
    roundTrip("""{"Sun":0}""", Body.Sun: Body)
  }
}
