package io.bullet.borer.site

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
      import io.bullet.borer.derivation.MapBasedCodecs._

      case class Foo(int: Int, string: String)
      case class Bar(foo: Foo, d: Double)

      implicit val fooCodec = deriveCodec[Foo]
      implicit val barCodec = deriveCodec[Bar]

      val foo = Foo(int = 42, string = "yeah")
      val bar = Bar(foo, d = 1.234)

      // Json.encode(bar).toByteArray or
      Json.encode(bar).toUtf8String ==>
      """{"foo":{"int":42,"string":"yeah"},"d":1.234}"""
      //#map-based
    }

    "Map-based details" - {
      //#example-adt
      sealed trait Animal
      case class Dog(age: Int, name: String)                      extends Animal
      case class Cat(weight: Double, color: String, home: String) extends Animal
      case class Fish(color: String)                              extends Animal
      case object Yeti                                            extends Animal
      //#example-adt

      {
        //#adt-codec-derivation
        import io.bullet.borer.derivation.MapBasedCodecs._

        implicit val animalCodec = {
          implicit val dogCodec  = deriveCodec[Dog]
          implicit val catCodec  = deriveCodec[Cat]
          implicit val fishCodec = deriveCodec[Fish]
          implicit val yetiCodec = deriveCodec[Yeti.type]
          deriveCodec[Animal]
        }
        //#adt-codec-derivation

        //#adt-default-encoding
        import io.bullet.borer.Json

        val animal: Animal = Dog(2, "Rex")

        // Json.encode(animal).toByteArray or
        Json.encode(animal).toUtf8String ==> """{"Dog":{"age":2,"name":"Rex"}}"""
        //#adt-default-encoding
      }

      {
        //#flat-adt-encoding
        import io.bullet.borer.{AdtEncodingStrategy, Json}
        import io.bullet.borer.derivation.MapBasedCodecs._

        // this enables the flat ADT encoding
        implicit val flatAdtEncoding =
          AdtEncodingStrategy.flat(typeMemberName = "_type")

        implicit val animalCodec = {
          implicit val dogCodec  = deriveCodec[Dog]
          implicit val catCodec  = deriveCodec[Cat]
          implicit val fishCodec = deriveCodec[Fish]
          implicit val yetiCodec = deriveCodec[Yeti.type]
          deriveCodec[Animal]
        }

        val animal: Animal = Dog(2, "Rex")

        // Json.encode(animal).toByteArray or
        Json.encode(animal).toUtf8String ==> """{"_type":"Dog","age":2,"name":"Rex"}"""
        //#flat-adt-encoding
      }
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
