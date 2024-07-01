/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.*
import MapBasedCodecs.*

import java.nio.charset.StandardCharsets

enum Body1 derives Codec:
  case Earth, Moon, Sun

///////////////////////////

enum Body2 derives Codec.All:
  case Earth, Moon
  case Asteroid(mass: Double)
  case Sun

///////////////////////////

sealed trait SuperBody3 derives Codec.All

enum Body3 extends SuperBody3:
  case Earth, Moon
  case Asteroid(mass: Double)
  case Sun

///////////////////////////

sealed trait SuperBody4 derives Codec.All

case class AltCase(x: Int) extends SuperBody4
case object AltSingle      extends SuperBody4

sealed trait SubBody extends SuperBody4

case class AltSub(x: Int) extends SubBody
case object AltSubSingle  extends SubBody

enum Body4 extends SuperBody4:
  case Earth, Moon
  case Asteroid(mass: Double)
  case Sun

///////////////////////////

sealed trait SuperBody5 derives Codec.All

object SuperBody5:
  case class AltCase(x: Int) extends SuperBody5
  case object AltSingle extends SuperBody5

  sealed trait SubBody extends SuperBody5 derives Codec.All
  case class AltSub(x: Int) extends SubBody
  case object AltSubSingle extends SubBody

  enum Body5 extends SuperBody5 derives Codec.All:
    case Earth, Moon, Sun
    case Asteroid(mass: Double)

///////////////////////////

class EnumSpec extends AbstractBorerSpec {

  def encode[T: Encoder](value: T): String   = Json.encode(value).toUtf8String
  def decode[T: Decoder](encoded: String): T = Json.decode(encoded getBytes StandardCharsets.UTF_8).to[T].value

  test("Body1") {
    roundTrip(""""Earth"""", Body1.Earth: Body1)
    roundTrip(""""Moon"""", Body1.Moon: Body1)
    roundTrip(""""Sun"""", Body1.Sun: Body1)
  }

  test("Body2") {
    roundTrip(""""Earth"""", Body2.Earth: Body2)
    roundTrip(""""Moon"""", Body2.Moon: Body2)
    roundTrip("""{"Asteroid":{"mass":18.3}}""", Body2.Asteroid(18.3): Body2)
    roundTrip(""""Sun"""", Body2.Sun: Body2)
  }

  test("Body3") {
    roundTrip(""""Earth"""", Body3.Earth: SuperBody3)
    roundTrip(""""Moon"""", Body3.Moon: SuperBody3)
    roundTrip("""{"Asteroid":{"mass":18.3}}""", Body3.Asteroid(18.3): SuperBody3)
    roundTrip(""""Sun"""", Body3.Sun: SuperBody3)
  }

  test("Body4") {
    roundTrip("""{"AltCase":{"x":42}}""", AltCase(42): SuperBody4)
    roundTrip("""{"AltSingle":{}}""", AltSingle: SuperBody4)
    roundTrip("""{"AltSub":{"x":18}}""", AltSub(18): SuperBody4)
    roundTrip("""{"AltSubSingle":{}}""", AltSubSingle: SuperBody4)
    roundTrip(""""Earth"""", Body4.Earth: SuperBody4)
    roundTrip(""""Moon"""", Body4.Moon: SuperBody4)
    roundTrip("""{"Asteroid":{"mass":18.3}}""", Body4.Asteroid(18.3): SuperBody4)
    roundTrip(""""Sun"""", Body4.Sun: SuperBody4)
  }

  test("Body5") {
    roundTrip("""{"AltCase":{"x":42}}""", SuperBody5.AltCase(42): SuperBody5)
    roundTrip("""{"AltSingle":{}}""", SuperBody5.AltSingle: SuperBody5)
    roundTrip("""{"AltSub":{"x":18}}""", SuperBody5.AltSub(18): SuperBody5)
    roundTrip("""{"AltSubSingle":{}}""", SuperBody5.AltSubSingle: SuperBody5)
    roundTrip(""""Earth"""", SuperBody5.Body5.Earth: SuperBody5)
    roundTrip(""""Moon"""", SuperBody5.Body5.Moon: SuperBody5)
    roundTrip("""{"Asteroid":{"mass":18.3}}""", SuperBody5.Body5.Asteroid(18.3): SuperBody5)
    roundTrip(""""Sun"""", SuperBody5.Body5.Sun: SuperBody5)
  }
}
