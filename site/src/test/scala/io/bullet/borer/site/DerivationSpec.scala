/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.site

import io.bullet.borer.BorerSuite

class DerivationSpec extends BorerSuite {

  case class Foo()

  {
    // #import-array-based
    import io.bullet.borer.derivation.ArrayBasedCodecs.*
    // #import-array-based
    deriveEncoder[Foo]
  }

  {
    // #import-map-based
    import io.bullet.borer.derivation.MapBasedCodecs.*
    // #import-map-based
    deriveEncoder[Foo]
  }

  test("Array-based") {

    // #array-based
    import io.bullet.borer.{Codec, Json}
    import io.bullet.borer.derivation.key
    import io.bullet.borer.derivation.ArrayBasedCodecs.*

    sealed trait Animal derives Codec
    case class Cat(weight: Double, color: String, home: String) extends Animal derives Codec
    @key("TheDog") case class Dog(age: Int, name: String)       extends Animal derives Codec
    @key(42) case class Mouse(tail: Boolean)                    extends Animal derives Codec

    val cat   = Cat(8.5, "grey", "sofa")
    val dog   = Dog(2, "Rex")
    val mouse = Mouse(tail = true)

    Json.encode(cat).toUtf8String ==> """[8.5,"grey","sofa"]"""
    Json.encode(dog).toUtf8String ==> """[2,"Rex"]"""
    Json.encode(mouse).toUtf8String ==> "true"

    def encodeAnimal(animal: Animal) = Json.encode(animal).toUtf8String

    encodeAnimal(cat) ==> """["Cat",[8.5,"grey","sofa"]]"""
    encodeAnimal(dog) ==> """["TheDog",[2,"Rex"]]"""
    encodeAnimal(mouse) ==> """[42,true]"""
    // #array-based
  }

  test("Map-based") {

    // #map-based
    import io.bullet.borer.{Json, Codec}
    import io.bullet.borer.derivation.MapBasedCodecs.*

    case class Foo(int: Int, string: String) derives Codec
    case class Bar(foo: Foo, d: Double) derives Codec

    val foo = Foo(int = 42, string = "yeah")
    val bar = Bar(foo, d = 1.234)

    // Json.encode(bar).toByteArray or
    Json.encode(bar).toUtf8String ==>
    """{"foo":{"int":42,"string":"yeah"},"d":1.234}"""
    // #map-based
  }

  test("Map-based details") {
    // #example-adt
    sealed trait Animal
    case class Dog(age: Int, name: String)                      extends Animal
    case class Cat(weight: Double, color: String, home: String) extends Animal
    case class Fish(color: String)                              extends Animal
    case object Yeti                                            extends Animal
    // #example-adt

    {
      // #adt-semi-automatic-codec-derivation
      import io.bullet.borer.Codec
      import io.bullet.borer.derivation.MapBasedCodecs.*

      given Codec[Animal] = {
        given Codec[Dog]       = deriveCodec[Dog]
        given Codec[Cat]       = deriveCodec[Cat]
        given Codec[Fish]      = deriveCodec[Fish]
        given Codec[Yeti.type] = deriveCodec[Yeti.type]
        deriveCodec[Animal]
      }
      // #adt-semi-automatic-codec-derivation

      // #adt-default-encoding
      import io.bullet.borer.Json

      val animal: Animal = Dog(2, "Rex")

      // Json.encode(animal).toByteArray or
      Json.encode(animal).toUtf8String ==> """{"Dog":{"age":2,"name":"Rex"}}"""
      // #adt-default-encoding
    }

    {
      // #adt-fully-automatic-codec-derivation
      import io.bullet.borer.Codec
      import io.bullet.borer.derivation.MapBasedCodecs.*

      given Codec[Animal] = deriveAllCodecs[Animal]
      // #adt-fully-automatic-codec-derivation

      import io.bullet.borer.Json

      val animal: Animal = Dog(2, "Rex")

      // Json.encode(animal).toByteArray or
      Json.encode(animal).toUtf8String ==> """{"Dog":{"age":2,"name":"Rex"}}"""
    }

    {
      // #derives
      import io.bullet.borer.{Codec, Json}
      import io.bullet.borer.derivation.MapBasedCodecs.*

      case class Color(r: Int, g: Int, b: Int) derives Codec

      val color = Color(255, 0, 0)

      Json.encode(color).toUtf8String ==> """{"r":255,"g":0,"b":0}"""
      // #derives
    }

    {
      // #derives-analog
      import io.bullet.borer.{Codec, Json}
      import io.bullet.borer.derivation.MapBasedCodecs.*

      case class Color(r: Int, g: Int, b: Int)

      object Color:
        given Codec[Color] = deriveCodec

      val color = Color(255, 0, 0)

      Json.encode(color).toUtf8String ==> """{"r":255,"g":0,"b":0}"""
      // #derives-analog
    }

    {
      // #derives-all
      import io.bullet.borer.{Codec, Json}
      import io.bullet.borer.derivation.MapBasedCodecs.*

      enum Body derives Codec.All:
        case Sun, Earth, Moon
        case Asteroid(mass: Double)

      val body: Body = Body.Asteroid(1.23)

      // Json.encode(body).toByteArray or
      Json.encode(body).toUtf8String ==> """{"Asteroid":{"mass":1.23}}"""
      // #derives-all
    }

    {
      // #derives-all-analog
      import io.bullet.borer.Codec
      import io.bullet.borer.derivation.MapBasedCodecs.*

      enum Body:
        case Sun, Earth, Moon
        case Asteroid(mass: Double)

      object Body:
        given Codec[Body] = deriveAllCodecs
      // #derives-all-analog
    }

    {
      // #flat-adt-encoding
      import io.bullet.borer.{AdtEncodingStrategy, Json, Codec}
      import io.bullet.borer.derivation.MapBasedCodecs.*

      // this enables the flat ADT encoding
      given AdtEncodingStrategy =
        AdtEncodingStrategy.flat(typeMemberName = "_type")

      given Codec[Animal] = deriveAllCodecs[Animal]

      val animal: Animal = Dog(2, "Rex")

      // Json.encode(animal).toByteArray or
      Json.encode(animal).toUtf8String ==> """{"_type":"Dog","age":2,"name":"Rex"}"""
      // #flat-adt-encoding
    }

    {
      // #custom-override
      import io.bullet.borer.{Codec, Json}
      import io.bullet.borer.derivation.MapBasedCodecs.*

      // custom codec only for `Fish`
      given Codec[Fish] =
        Codec.bimap[Int, Fish](_ => 0, _ => Fish("red"))

      // let borer derive the codecs for the rest of the Animal ADT
      given Codec[Animal] = deriveAllCodecs

      val animal: Animal = Fish("blue")

      Json.encode(animal).toUtf8String ==> """{"Fish":0}"""
      // #custom-override
    }

    {
      // #adt-dog-cat-derivation
      import io.bullet.borer.Codec
      import io.bullet.borer.derivation.MapBasedCodecs.*

      given Codec[Dog]    = deriveCodec[Dog]
      given Codec[Cat]    = deriveCodec[Cat]
      given Codec[Animal] = deriveAllCodecs[Animal]
      // #adt-dog-cat-derivation

      // format: OFF
      //#animal-vs-dog
      import io.bullet.borer.Json

      val dog = Dog(2, "Rex")
      Json.encode(dog: Animal).toUtf8String ==> """{"Dog":{"age":2,"name":"Rex"}}"""
      Json.encode(dog        ).toUtf8String ==> """{"age":2,"name":"Rex"}"""
      //#animal-vs-dog
      // format: ON
    }
  }

  test("Default Value") {
    // #default-value
    import io.bullet.borer.{Json, Codec}
    import io.bullet.borer.derivation.MapBasedCodecs.*

    case class Dog(age: Int, name: String = "<unknown>") derives Codec

    Json
      .decode("""{ "age": 4 }""" getBytes "UTF8")
      .to[Dog]
      .value ==> Dog(age = 4)
    // #default-value
  }

  test("Custom Member Name") {
    // #custom-member-name
    import io.bullet.borer.{Json, Codec}
    import io.bullet.borer.derivation.key
    import io.bullet.borer.derivation.MapBasedCodecs.*

    case class Dog(age: Int, @key("the-name") name: String) derives Codec

    Json.encode(Dog(1, "Lolle")).toUtf8String ==>
    """{"age":1,"the-name":"Lolle"}"""
    // #custom-member-name
  }

  test("Recursive ADT old") {
    // #recursive-adt-old
    import io.bullet.borer.Codec
    import io.bullet.borer.derivation.MapBasedCodecs.*

    sealed trait TreeNode
    case object Leaf                                 extends TreeNode
    case class Node(left: TreeNode, right: TreeNode) extends TreeNode

    implicit lazy val codec: Codec[TreeNode] = deriveAllCodecs
    // #recursive-adt-old
  }

  test("Recursive ADT new") {
    // #recursive-adt-new
    import io.bullet.borer.Codec
    import io.bullet.borer.derivation.MapBasedCodecs.*

    sealed trait TreeNode
    case object Leaf                                 extends TreeNode
    case class Node(left: TreeNode, right: TreeNode) extends TreeNode

    given Codec[TreeNode] = deriveAllCodecs
    // #recursive-adt-new
  }
}
