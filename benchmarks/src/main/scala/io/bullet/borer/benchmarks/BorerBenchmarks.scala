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

  implicit val fooCodec    = deriveCodec[Foo]
  implicit val foosEncoder = implicitly[Encoder[Map[String, Foo]]]
  implicit val foosDecoder = implicitly[Decoder[Map[String, Foo]]]

  implicit val intsEncoder = implicitly[Encoder[List[Int]]]
  implicit val intsDecoder = implicitly[Decoder[List[Int]]]
}

import BorerCodecs._

class BorerEncodingBenchmark extends EncodingBenchmark {

  @Benchmark
  def encodeFoosBorerV: Array[Byte] = Json.encode(foos).toByteArray

  @Benchmark
  def encodeFoosBorerNV: Array[Byte] =
    Json.encode(foos).withConfig(Writer.Config.defaultWithoutValidation).toByteArray

  @Benchmark
  def encodeIntsBorerV: Array[Byte] = Json.encode(ints).toByteArray

  @Benchmark
  def encodeIntsBorerNV: Array[Byte] =
    Json.encode(ints).withConfig(Writer.Config.defaultWithoutValidation).toByteArray
}

class BorerDecodingBenchmark extends DecodingBenchmark {

  @Benchmark
  def decodeFoosBorerV: Map[String, Foo] = Json.decode(foosJson).to[Map[String, Foo]].value

  @Benchmark
  def decodeFoosBorerNV: Map[String, Foo] =
    Json.decode(foosJson).withConfig(Reader.Config.defaultWithoutValidation).to[Map[String, Foo]].value

  @Benchmark
  def decodeIntsBorerV: List[Int] = Json.decode(intsJson).to[List[Int]].value

  @Benchmark
  def decodeIntsBorerNV: List[Int] =
    Json.decode(intsJson).withConfig(Reader.Config.defaultWithoutValidation).to[List[Int]].value
}

class BorerDomBenchmark extends DomBenchmark {

  private var root: Dom.Element = _
  def setup(): Unit             = root = Json.decode(fileBytes).to[Dom.Element].value

  @Benchmark
  def encodeDomBorer: Array[Byte] = Json.encode(root).toByteArray

  @Benchmark
  def decodeDomBorer: Dom.Element = Json.decode(fileBytes).to[Dom.Element].value
}
