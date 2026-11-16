/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.*

import java.nio.charset.StandardCharsets

object EnumCasesAsObjectsSpec:

  enum Planet:
    case Mercury, Venus, Earth

  enum Body:
    case Earth, Moon
    case Asteroid(mass: Double)
    case Sun

  sealed trait Thing
  case class Rock(weight: Int) extends Thing
  case object Void             extends Thing

  enum Celestial extends Thing:
    case Star, Comet
    case Planetoid(size: Int)

  object MapDefault:
    given DerivationConfig = DerivationConfig(encodeEnumCasesAsObjects = true)
    import MapBasedCodecs.*
    given Codec[Planet] = deriveCodec
    given Codec[Body]   = deriveAllCodecs
    given Codec[Thing]  = deriveAllCodecs

  object MapFlat:
    given DerivationConfig = DerivationConfig(encodeEnumCasesAsObjects = true, flatAdtEncoding = true)
    import MapBasedCodecs.*
    given Codec[Planet] = deriveCodec
    given Codec[Body]   = deriveAllCodecs
    given Codec[Thing]  = deriveAllCodecs

  object ArrayBased:
    given DerivationConfig = DerivationConfig(encodeEnumCasesAsObjects = true)
    import ArrayBasedCodecs.*
    given Codec[Planet] = deriveCodec
    given Codec[Body]   = deriveAllCodecs
    given Codec[Thing]  = deriveAllCodecs

  object MapNoFlag:
    import MapBasedCodecs.*
    given Codec[Planet] = deriveCodec
    given Codec[Body]   = deriveAllCodecs
    given Codec[Thing]  = deriveAllCodecs

  object ArrayNoFlag:
    import ArrayBasedCodecs.*
    given Codec[Planet] = deriveCodec
    given Codec[Body]   = deriveAllCodecs

class EnumCasesAsObjectsSpec extends AbstractBorerSpec:
  import EnumCasesAsObjectsSpec.*

  def encode[T: Encoder](value: T): String   = Json.encode(value).toUtf8String
  def decode[T: Decoder](encoded: String): T = Json.decode(encoded.getBytes(StandardCharsets.UTF_8)).to[T].value

  test("map-based, default ADT encoding") {
    import MapDefault.given
    roundTrip("""{"Earth":{}}""", Planet.Earth)
    roundTrip("""{"Moon":{}}""", Body.Moon: Body)
    roundTrip("""{"Asteroid":{"mass":1.5}}""", Body.Asteroid(1.5): Body)
    roundTrip("""{"Rock":{"weight":3}}""", Rock(3): Thing)
    roundTrip("""{"Void":{}}""", Void: Thing)
    roundTrip("""{"Star":{}}""", Celestial.Star: Thing)
    roundTrip("""{"Planetoid":{"size":7}}""", Celestial.Planetoid(7): Thing)

    // bare type ids are still accepted
    verifyDecoding(""""Earth"""", Planet.Earth)
    verifyDecoding(""""Sun"""", Body.Sun: Body)
    verifyDecoding(""""Comet"""", Celestial.Comet: Thing)

    // non-empty maps are tolerated, like for case objects
    verifyDecoding("""{"Earth":{"foo":42}}""", Planet.Earth)
  }

  test("map-based, default ADT encoding, CBOR") {
    import MapDefault.given
    val bytes = Cbor.encode(Body.Moon: Body).toByteArray
    toHexString(bytes) ==> "a1644d6f6f6ea0"
    Cbor.decode(bytes).to[Body].value ==> Body.Moon
    Cbor.decode(Cbor.encode("Moon").toByteArray).to[Body].value ==> Body.Moon
  }

  test("map-based, flat ADT encoding") {
    import MapFlat.given
    roundTrip("""{"_type":"Earth"}""", Planet.Earth)
    roundTrip("""{"_type":"Moon"}""", Body.Moon: Body)
    roundTrip("""{"_type":"Asteroid","mass":1.5}""", Body.Asteroid(1.5): Body)
    roundTrip("""{"_type":"Void"}""", Void: Thing)
    roundTrip("""{"_type":"Star"}""", Celestial.Star: Thing)
    roundTrip("""{"_type":"Planetoid","size":7}""", Celestial.Planetoid(7): Thing)
    verifyDecoding(""""Earth"""", Planet.Earth)
    verifyDecoding(""""Comet"""", Celestial.Comet: Thing)
  }

  test("array-based") {
    import ArrayBased.given
    roundTrip("""["Earth",[]]""", Planet.Earth)
    roundTrip("""["Moon",[]]""", Body.Moon: Body)
    roundTrip("""["Asteroid",1.5]""", Body.Asteroid(1.5): Body)
    roundTrip("""["Void",[]]""", Void: Thing)
    roundTrip("""["Star",[]]""", Celestial.Star: Thing)
    verifyDecoding(""""Earth"""", Planet.Earth)
    verifyDecoding(""""Comet"""", Celestial.Comet: Thing)
  }

  test("flag disabled: unchanged encoding, but object form is accepted during decoding") {
    {
      import MapNoFlag.given
      roundTrip(""""Earth"""", Planet.Earth)
      roundTrip(""""Moon"""", Body.Moon: Body)
      roundTrip(""""Star"""", Celestial.Star: Thing)
      verifyDecoding("""{"Earth":{}}""", Planet.Earth)
      verifyDecoding("""{"Moon":{}}""", Body.Moon: Body)
      verifyDecoding("""{"Star":{}}""", Celestial.Star: Thing)
    }
    {
      import ArrayNoFlag.given
      roundTrip(""""Earth"""", Planet.Earth)
      verifyDecoding("""["Earth",[]]""", Planet.Earth)
      verifyDecoding("""["Moon",[]]""", Body.Moon: Body)
    }
  }
