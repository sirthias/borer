/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.*
import io.bullet.borer.derivation.DerivationConfig.Naming

import java.nio.charset.StandardCharsets
import scala.annotation.unused

object NamingSpec:

  case class Person(firstName: String, httpURL: String, @key("ID") userId: Int, age2: Int = 42)

  sealed trait Animal
  case class BigDog(barkLevel: Int)   extends Animal
  case object TinyCat                 extends Animal
  @key("FISH") case class GoldFish()  extends Animal
  @key(7) case class SeaHorse(x: Int) extends Animal

  enum Planet:
    case RedGiant, WhiteDwarf
    case GasPlanet(ringCount: Int)

  object SharedConfig:
    inline given DerivationConfig = DerivationConfig().withNaming(Naming.snake_case)

  object MapBased:
    inline given DerivationConfig =
      DerivationConfig().withMemberNaming(Naming.snake_case).withAdtTypeIdNaming(Naming.`kebab-case`)
    import MapBasedCodecs.*
    given Codec[Person] = deriveCodec
    given Codec[Animal] = deriveAllCodecs
    given Codec[Planet] = deriveAllCodecs

  object MapBasedFlat:
    inline given DerivationConfig = DerivationConfig(
      flatAdtEncoding = Some(DerivationConfig.FlatAdtEncoding("typeMember", 1024)),
      memberNaming = Naming.SNAKE_CASE,
      adtTypeIdNaming = Naming.camelCase)
    import MapBasedCodecs.*
    given Codec[Animal] = deriveAllCodecs
    given Codec[Planet] = deriveAllCodecs

  object ArrayBased:
    inline given DerivationConfig =
      DerivationConfig().withNaming(Naming.PascalCase).withAdtTypeIdNaming(Naming.SNAKE_CASE)
    import ArrayBasedCodecs.*
    given Codec[Person] = deriveCodec
    given Codec[Animal] = deriveAllCodecs
    given Codec[Planet] = deriveAllCodecs

  object Shared:
    import SharedConfig.given
    import MapBasedCodecs.*
    given Codec[Person] = deriveCodec
    given Codec[Animal] = deriveAllCodecs

class NamingSpec extends AbstractBorerSpec:
  import NamingSpec.*

  def encode[T: Encoder](value: T): String   = Json.encode(value).toUtf8String
  def decode[T: Decoder](encoded: String): T = Json.decode(encoded.getBytes(StandardCharsets.UTF_8)).to[T].value

  test("Naming transformations") {
    val names = List(
      "fooBar",
      "FooBar",
      "foo",
      "httpURL",
      "HTTPServer",
      "userID",
      "v2Api",
      "field1",
      "foo_bar",
      "foo-bar",
      "_myType",
      "x",
      "ABC",
      "_")
    def row(n: Naming) = names.map(n(_))

    row(Naming.Unchanged) ==> names
    row(Naming.camelCase) ==> List(
      "fooBar",
      "fooBar",
      "foo",
      "httpUrl",
      "httpServer",
      "userId",
      "v2Api",
      "field1",
      "fooBar",
      "fooBar",
      "_myType",
      "x",
      "abc",
      "_")
    row(Naming.PascalCase) ==> List(
      "FooBar",
      "FooBar",
      "Foo",
      "HttpUrl",
      "HttpServer",
      "UserId",
      "V2Api",
      "Field1",
      "FooBar",
      "FooBar",
      "_MyType",
      "X",
      "Abc",
      "_")
    row(Naming.snake_case) ==> List(
      "foo_bar",
      "foo_bar",
      "foo",
      "http_url",
      "http_server",
      "user_id",
      "v2_api",
      "field1",
      "foo_bar",
      "foo_bar",
      "_my_type",
      "x",
      "abc",
      "_")
    row(Naming.SNAKE_CASE) ==> List(
      "FOO_BAR",
      "FOO_BAR",
      "FOO",
      "HTTP_URL",
      "HTTP_SERVER",
      "USER_ID",
      "V2_API",
      "FIELD1",
      "FOO_BAR",
      "FOO_BAR",
      "_MY_TYPE",
      "X",
      "ABC",
      "_")
    row(Naming.`kebab-case`) ==> List(
      "foo-bar",
      "foo-bar",
      "foo",
      "http-url",
      "http-server",
      "user-id",
      "v2-api",
      "field1",
      "foo-bar",
      "foo-bar",
      "_my-type",
      "x",
      "abc",
      "_")
  }

  test("map-based, default ADT encoding") {
    import MapBased.given
    roundTrip("""{"first_name":"Ann","http_url":"x","ID":1}""", Person("Ann", "x", 1))
    roundTrip("""{"first_name":"Ann","http_url":"x","ID":1,"age2":3}""", Person("Ann", "x", 1, 3))
    roundTrip("""{"big-dog":{"bark_level":3}}""", BigDog(3): Animal)
    roundTrip("""{"tiny-cat":{}}""", TinyCat: Animal)
    roundTrip("""{"FISH":{}}""", GoldFish(): Animal)
    roundTrip(""""red-giant"""", Planet.RedGiant)
    roundTrip("""{"gas-planet":{"ring_count":2}}""", Planet.GasPlanet(2): Planet)

    // the untransformed names are not accepted anymore
    intercept[Borer.Error.InvalidInputData[?]](decode[Planet](""""RedGiant""""))
    intercept[Borer.Error.InvalidInputData[?]](decode[Animal]("""{"BigDog":{"bark_level":3}}"""))
  }

  test("map-based, flat ADT encoding") {
    import MapBasedFlat.given
    roundTrip("""{"typeMember":"bigDog","BARK_LEVEL":3}""", BigDog(3): Animal)
    roundTrip("""{"typeMember":"tinyCat"}""", TinyCat: Animal)
    roundTrip("""{"typeMember":7,"X":5}""", SeaHorse(5): Animal)
    roundTrip(""""whiteDwarf"""", Planet.WhiteDwarf)
    roundTrip("""{"typeMember":"gasPlanet","RING_COUNT":2}""", Planet.GasPlanet(2): Planet)
  }

  test("array-based") {
    import ArrayBased.given
    roundTrip("""["Ann","x",1,42]""", Person("Ann", "x", 1))
    roundTrip("""["BIG_DOG",3]""", BigDog(3): Animal)
    roundTrip("""["TINY_CAT",[]]""", TinyCat: Animal)
    roundTrip(""""RED_GIANT"""", Planet.RedGiant)
  }

  test("config from other scope") {
    import Shared.given
    roundTrip("""{"first_name":"Ann","http_url":"x","ID":1}""", Person("Ann", "x", 1))
    roundTrip("""{"big_dog":{"bark_level":3}}""", BigDog(3): Animal)
  }

  test("naming-induced key collisions") {
    @unused case class Foo(fooBar: Int, foo_bar: Int)
    inline given DerivationConfig = DerivationConfig().withNaming(Naming.snake_case)
    val errors                    = compileErrors("MapBasedCodecs.deriveDecoder[Foo]")
    assert(
      errors.contains("share the same type id `foo_bar` (after applying the configured `snake_case` naming)"),
      errors)
  }

  test("non-inline given") {
    @unused case class Foo(x: Int)
    @unused given DerivationConfig = DerivationConfig().withNaming(Naming.snake_case)
    val errors                     = compileErrors("MapBasedCodecs.deriveEncoder[Foo]")
    assert(errors.contains("is not an `inline given`"), errors)
  }

  test("non-static config value") {
    @unused case class Foo(x: Int)
    val naming                    = Naming.snake_case
    inline given DerivationConfig = DerivationConfig().withNaming(naming)
    val errors                    = compileErrors("MapBasedCodecs.deriveEncoder[Foo]")
    assert(errors.contains("Cannot statically evaluate the given `DerivationConfig`"), errors)
  }

  test("invalid config") {
    sealed trait Foo
    case class Bar(x: Int) extends Foo
    @unused given Codec[Bar] = MapBasedCodecs.deriveCodec
    locally {
      inline given DerivationConfig = DerivationConfig().withFlatAdtEncoding(maxBufferSize = 1000)
      val errors                    = compileErrors("MapBasedCodecs.deriveEncoder[Foo]")
      assert(
        errors.contains("Invalid `DerivationConfig`: requirement failed: maxBufferSize must be a power of 2"),
        errors)
    }
  }

  test("array-based case class derivation doesn't require a static config") {
    case class Foo(x: Int)
    given DerivationConfig = DerivationConfig()
    given Codec[Foo]       = ArrayBasedCodecs.deriveCodec
    roundTrip("""3""", Foo(3))
  }
