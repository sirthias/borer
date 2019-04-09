/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

import io.bullet.borer._
import org.openjdk.jmh.annotations._

object BorerCodecs {
  import io.bullet.borer.derivation.MapBasedCodecs._

  implicit val intsEncoder = implicitly[Encoder[List[Int]]]
  implicit val intsDecoder = implicitly[Decoder[List[Int]]]

  object Derived {
    implicit val fooCodec    = deriveCodec[Foo]
    implicit val foosEncoder = implicitly[Encoder[Map[String, Foo]]]
    implicit val foosDecoder = implicitly[Decoder[Map[String, Foo]]]
  }

  object Manual {
    implicit val fooCodec = Codec.of[Foo](
      encode = (w, x) ⇒ {
        w.writeMapStart()
          .writeString("string")
          .writeString(x.string)
          .writeString("double")
          .writeDouble(x.double)
          .writeString("int")
          .writeInt(x.int)
          .writeString("long")
          .writeLong(x.long)
        x.listOfBools.foldLeft(w.writeString("listOfBools").writeArrayStart())(_ writeBool _).writeBreak().writeBreak()
      },
      decode = { r ⇒
        r.readMapStart()
        val foo = Foo(
          r.readString("string").readString(),
          r.readString("double").readDouble(),
          r.readString("int").readInt(),
          r.readString("long").readLong(),
          r.readString("listOfBools").read[List[Boolean]]()
        )
        r.readBreak()
        foo
      }
    )
    implicit val foosEncoder = implicitly[Encoder[Map[String, Foo]]]
    implicit val foosDecoder = implicitly[Decoder[Map[String, Foo]]]
  }
}

import BorerCodecs._

class BorerEncodingBenchmark extends EncodingBenchmark {

  @Benchmark
  def encodeFoosDV: Array[Byte] = Json.encode(foos)(Derived.foosEncoder).toByteArray

  @Benchmark
  def encodeFoosDNV: Array[Byte] =
    Json
      .encode(foos)(Derived.foosEncoder)
      .withConfig(Writer.Config.defaultWithoutValidation)
      .toByteArray

  @Benchmark
  def encodeFoosMV: Array[Byte] = Json.encode(foos)(Manual.foosEncoder).toByteArray

  @Benchmark
  def encodeFoosMNV: Array[Byte] =
    Json.encode(foos)(Manual.foosEncoder).withConfig(Writer.Config.defaultWithoutValidation).toByteArray

  @Benchmark
  def encodeIntsV: Array[Byte] = Json.encode(ints).toByteArray

  @Benchmark
  def encodeIntsNV: Array[Byte] =
    Json.encode(ints).withConfig(Writer.Config.defaultWithoutValidation).toByteArray

  @Benchmark
  def encodeEmptyArray: Array[Byte] =
    Json.encode(List.empty[Int]).withConfig(Writer.Config.defaultWithoutValidation).toByteArray
}

class BorerDecodingBenchmark extends DecodingBenchmark {

  @Benchmark
  def decodeFoosDV: Map[String, Foo] = Json.decode(foosJson).to[Map[String, Foo]](Derived.foosDecoder).value

  @Benchmark
  def decodeFoosDNV: Map[String, Foo] =
    Json
      .decode(foosJson)
      .withConfig(Reader.Config.defaultWithoutValidation)
      .to[Map[String, Foo]](Derived.foosDecoder)
      .value

  @Benchmark
  def decodeFoosMV: Map[String, Foo] = Json.decode(foosJson).to[Map[String, Foo]](Manual.foosDecoder).value

  @Benchmark
  def decodeFoosMNV: Map[String, Foo] =
    Json
      .decode(foosJson)
      .withConfig(Reader.Config.defaultWithoutValidation)
      .to[Map[String, Foo]](Manual.foosDecoder)
      .value

  @Benchmark
  def decodeIntsV: List[Int] = Json.decode(intsJson).to[List[Int]].value

  @Benchmark
  def decodeIntsNV: List[Int] =
    Json.decode(intsJson).withConfig(Reader.Config.defaultWithoutValidation).to[List[Int]].value

  @Benchmark
  def decodeEmptyArray: List[Int] =
    Json.decode(emptyArrayJson).withConfig(Reader.Config.defaultWithoutValidation).to[List[Int]].value
}

class BorerDomBenchmark extends DomBenchmark {

  private var root: Dom.Element = _
  def setup(): Unit             = root = Json.decode(fileBytes).to[Dom.Element].value

  @Benchmark
  def encodeDom: Array[Byte] =
    Json.encode(root).withConfig(Writer.Config.defaultWithoutValidation).toByteArray

  @Benchmark
  def decodeDom: Dom.Element =
    Json.decode(fileBytes).withConfig(Reader.Config.defaultWithoutValidation).to[Dom.Element].value
}
