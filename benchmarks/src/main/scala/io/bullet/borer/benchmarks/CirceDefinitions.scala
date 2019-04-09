/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

import java.nio.charset.StandardCharsets.UTF_8

import io.circe._
import io.circe.jawn.{decode, parse}
import org.openjdk.jmh.annotations._

object CirceCodecs {
  implicit val fooEncoder: Encoder[Foo] = io.circe.derivation.deriveEncoder
  implicit val fooDecoder: Decoder[Foo] = io.circe.derivation.deriveDecoder

  implicit val foosEncoder = implicitly[Encoder[Map[String, Foo]]]
  implicit val foosDecoder = implicitly[Decoder[Map[String, Foo]]]

  implicit val intsEncoder = implicitly[Encoder[List[Int]]]
  implicit val intsDecoder = implicitly[Decoder[List[Int]]]
}

import io.bullet.borer.benchmarks.CirceCodecs._

class CirceEncodingBenchmark extends EncodingBenchmark {

  @Benchmark
  def encodeFoos: Array[Byte] = foosEncoder(foos).noSpaces.getBytes(UTF_8)

  @Benchmark
  def encodeInts: Array[Byte] = intsEncoder(ints).noSpaces.getBytes(UTF_8)

  @Benchmark
  def encodeEmptyArray: Array[Byte] = intsEncoder(Nil).noSpaces.getBytes(UTF_8)
}

class CirceDecodingBenchmark extends DecodingBenchmark {

  @Benchmark
  def decodeFoos: Map[String, Foo] = decode[Map[String, Foo]](new String(foosJson, UTF_8)).right.get

  @Benchmark
  def decodeInts: List[Int] = decode[List[Int]](new String(intsJson, UTF_8)).right.get

  @Benchmark
  def decodeEmptyArray: List[Int] = decode[List[Int]](new String(emptyArrayJson, UTF_8)).right.get
}

class CirceDomBenchmark extends DomBenchmark {

  private var root: Json = _
  def setup(): Unit      = root = parse(new String(fileBytes, UTF_8)).right.get

  @Benchmark
  def encodeDom: Array[Byte] = root.noSpaces.getBytes(UTF_8)

  @Benchmark
  def decodeDom: Json = parse(new String(fileBytes, UTF_8)).right.get
}
