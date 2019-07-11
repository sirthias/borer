package io.bullet.borer.site

import io.bullet.borer.Borer
import utest._

object DerivationSpec extends TestSuite {

  case class Foo()

  {
    //#import-array-based
    import io.bullet.borer.derivation.ArrayBasedCodecs._
    //#import-array-based
    deriveEncoder[Foo]
  }

  {
    //#import-map-based
    import io.bullet.borer.derivation.MapBasedCodecs._
    //#import-map-based
    deriveEncoder[Foo]
  }

  val tests = Tests {

    "Array-based" - {

      //#array-based
      import io.bullet.borer.Json
      import io.bullet.borer.derivation.key
      import io.bullet.borer.derivation.ArrayBasedCodecs._

      sealed trait Animal
      case class Cat(weight: Double, color: String, home: String) extends Animal
      @key("TheDog") case class Dog(age: Int, name: String)       extends Animal
      @key(42) case class Mouse(tail: Boolean)                    extends Animal

      implicit val dogCodec   = deriveCodec[Dog]
      implicit val catCodec   = deriveCodec[Cat]
      implicit val mouseCodec = deriveCodec[Mouse]

      implicit val animalCodec = deriveCodec[Animal]

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
      //#array-based
    }

    "Map-based" - {

      //#map-based
      import io.bullet.borer.Json
      import io.bullet.borer.derivation.key
      import io.bullet.borer.derivation.MapBasedCodecs._

      sealed trait Animal
      case class Cat(weight: Double, color: String, home: String) extends Animal
      @key("TheDog") case class Dog(age: Int, name: String)       extends Animal
      @key(42) case class Mouse(tail: Boolean)                    extends Animal

      implicit val dogCodec   = deriveCodec[Dog]
      implicit val catCodec   = deriveCodec[Cat]
      implicit val mouseCodec = deriveCodec[Mouse]

      implicit val animalCodec = deriveCodec[Animal]

      val cat   = Cat(8.5, "grey", "sofa")
      val dog   = Dog(2, "Rex")
      val mouse = Mouse(tail = true)

      Json.encode(cat).toUtf8String ==>
      """{"weight":8.5,"color":"grey","home":"sofa"}"""

      Json.encode(dog).toUtf8String ==>
      """{"age":2,"name":"Rex"}"""

      Json.encode(mouse).toUtf8String ==>
      """{"tail":true}"""

      def encodeAnimal(animal: Animal) = Json.encode(animal).toUtf8String

      encodeAnimal(cat) ==> """{"Cat":{"weight":8.5,"color":"grey","home":"sofa"}}"""
      encodeAnimal(dog) ==> """{"TheDog":{"age":2,"name":"Rex"}}"""

      intercept[Borer.Error.ValidationFailure[_]] { encodeAnimal(mouse) }.getMessage ==>
      "JSON does not support integer values as a map key (Output.ToByteArray index 1)"
      //#map-based
    }

    "Default Value" - {
      //#default-value
      import io.bullet.borer.Json
      import io.bullet.borer.derivation.MapBasedCodecs._

      case class Dog(age: Int, name: String = "<unknown>")

      implicit val dogCodec = deriveCodec[Dog]

      Json
        .decode("""{ "age": 4 }""" getBytes "UTF8")
        .to[Dog]
        .value ==> Dog(age = 4)
      //#default-value
    }

    "Custom Member Name" - {
      //#custom-member-name
      import io.bullet.borer.Json
      import io.bullet.borer.derivation.key
      import io.bullet.borer.derivation.MapBasedCodecs._

      case class Dog(age: Int, @key("the-name") name: String)

      implicit val dogCodec = deriveCodec[Dog]

      Json.encode(Dog(1, "Lolle")).toUtf8String ==>
      """{"age":1,"the-name":"Lolle"}"""
      //#custom-member-name
    }
  }
}
