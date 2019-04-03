/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

import java.nio.charset.StandardCharsets.UTF_8
import org.openjdk.jmh.annotations._

object UPickleCodecs {
  implicit val fooRW: upickle.default.ReadWriter[Foo] = upickle.default.macroRW

  implicit val foosWriter = implicitly[upickle.default.Writer[Map[String, Foo]]]
  implicit val intsWriter = implicitly[upickle.default.Writer[List[Int]]]

  implicit val foosReader = implicitly[upickle.default.Reader[Map[String, Foo]]]
  implicit val intsReader = implicitly[upickle.default.Reader[List[Int]]]
}

import UPickleCodecs._

class UPickleEncodingBenchmark extends EncodingBenchmark {

  @Benchmark
  def encodeFoosUPickle: Array[Byte] = upickle.default.write(foos).getBytes(UTF_8)

  @Benchmark
  def encodeIntsUPickle: Array[Byte] = upickle.default.write(ints).getBytes(UTF_8)
}

class UPickleDecodingBenchmark extends DecodingBenchmark {

  @Benchmark
  def decodeFoosUPickle: Map[String, Foo] = upickle.default.read[Map[String, Foo]](new String(foosJson, UTF_8))

  @Benchmark
  def decodeIntsUPickle: List[Int] = upickle.default.read[List[Int]](new String(intsJson, UTF_8))
}

class UPickleDomBenchmark extends DomBenchmark {

  private var root: ujson.Value = _
  def setup(): Unit             = root = upickle.default.read[ujson.Value](new String(fileBytes, UTF_8))

  @Benchmark
  def encodeDomUPickle: Array[Byte] = upickle.default.write(root).getBytes(UTF_8)

  @Benchmark
  def decodeDomUPickle: ujson.Value = upickle.default.read[ujson.Value](new String(fileBytes, UTF_8))
}
