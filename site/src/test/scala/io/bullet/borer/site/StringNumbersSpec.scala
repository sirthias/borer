package io.bullet.borer.site

import utest._

object StringNumbersSpec extends TestSuite {

  val tests = Tests {

    "Imports" - {
      //#imports
      import io.bullet.borer.{Decoder, Encoder}

      import Encoder.StringNumbers._  // enables number-as-strings encoding
      import Encoder.StringBooleans._ // enables booleans-as-strings encoding
      import Encoder.StringNulls._    // enables null-as-strings encoding

      import Decoder.StringNumbers._  // enables number-as-strings decoding
      import Decoder.StringBooleans._ // enables booleans-as-strings decoding
      import Decoder.StringNulls._    // enables null-as-strings decoding
      //#imports

      intEncoder.hashCode() ^
      intDecoder.hashCode() ^
      booleanEncoder.hashCode() ^
      booleanDecoder.hashCode() ^
      nullEncoder.hashCode() ^
      nullDecoder.hashCode()
    }

    "Example" - {
      //#example
      import io.bullet.borer.{Encoder, Json}
      import io.bullet.borer.derivation.MapBasedCodecs._

      case class Dog(age: Int, male: Boolean, name: String)

      import Encoder.StringNumbers._  // enables number-as-strings encoding
      import Encoder.StringBooleans._ // enables booleans-as-strings encoding

      implicit val dogCodec = deriveCodec[Dog]

      val dog = Dog(2, false, "Lolle")

      // note how the Int and Boolean are rendered as JSON strings
      Json.encode(dog).toUtf8String ==>
      """{"age":"2","male":"false","name":"Lolle"}"""
      //#example
    }
  }
}
