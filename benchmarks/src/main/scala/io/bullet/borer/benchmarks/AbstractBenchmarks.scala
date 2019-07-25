/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

import java.util.concurrent.TimeUnit
import java.nio.charset.StandardCharsets.UTF_8

import cats.kernel.Eq
import org.openjdk.jmh.annotations._

import scala.io.Source
import scala.util.Random

/**
  * Compares the performance of encoding operations.
  *
  * The following command will run the benchmarks with reasonable settings:
  *
  * > sbt "jmh:run -i 10 -wi 10 -f 2 -t 1 io.bullet.borer.benchmarks.*EncodingBenchmark"
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(
  value = 1,
  jvmArgs = Array(
    "-server",
    "-Xms2g",
    "-Xmx2g",
    "-XX:NewSize=1g",
    "-XX:MaxNewSize=1g",
    "-XX:InitialCodeCacheSize=512m",
    "-XX:ReservedCodeCacheSize=512m",
    "-XX:+UseParallelGC",
    "-XX:-UseBiasedLocking",
    "-XX:+AlwaysPreTouch"))
abstract class EncodingBenchmark extends EncodingDecodingExampleData

/**
  * Compares the performance of decoding operations.
  *
  * The following command will run the benchmarks with reasonable settings:
  *
  * > sbt "jmh:run -i 10 -wi 10 -f 2 -t 1 io.bullet.borer.benchmarks.*DecodingBenchmark"
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(
  value = 1,
  jvmArgs = Array(
    "-server",
    "-Xms2g",
    "-Xmx2g",
    "-XX:NewSize=1g",
    "-XX:MaxNewSize=1g",
    "-XX:InitialCodeCacheSize=512m",
    "-XX:ReservedCodeCacheSize=512m",
    "-XX:+UseParallelGC",
    "-XX:-UseBiasedLocking",
    "-XX:+AlwaysPreTouch"))
abstract class DecodingBenchmark extends EncodingDecodingExampleData

/**
  * Compares the performance of encoding and decoding from and to the respective DOM representations.
  *
  * The following command will run the benchmarks with reasonable settings:
  *
  * > sbt "jmh:run -i 10 -wi 10 -f 2 -t 1 io.bullet.borer.benchmarks.*DomBenchmark"
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(
  value = 1,
  jvmArgs = Array(
    "-server",
    "-Xms2g",
    "-Xmx2g",
    "-XX:NewSize=1g",
    "-XX:MaxNewSize=1g",
    "-XX:InitialCodeCacheSize=512m",
    "-XX:ReservedCodeCacheSize=512m",
    "-XX:+UseParallelGC",
    "-XX:-UseBiasedLocking",
    "-XX:+AlwaysPreTouch"))
abstract class DomBenchmark {

  @Param(
    Array(
      "australia-abc.json",
      "bitcoin.json",
      "doj-blog.json",
      "eu-lobby-country.json",
      "eu-lobby-financial.json",
      "eu-lobby-repr.json",
      "github-events.json",
      "github-gists.json",
      "json-generator.json",
      "meteorites.json",
      "movies.json",
      "reddit-scala.json",
      "rick-morty.json",
      "temp-anomaly.json",
      "thai-cinemas.json",
      "turkish.json",
      "twitter_api_compact_response.json",
      "twitter_api_response.json"))
  var fileName: String = _

  var fileBytes: Array[Byte] = _

  @Setup
  def load(): Unit = {
    fileBytes = Source.fromResource(fileName).mkString.getBytes(UTF_8)
    setup()
  }

  def setup(): Unit
}

final case class Foo(string: String, double: Double, int: Int, long: Long, listOfBools: List[Boolean])

object Foo {
  implicit val eqFoo: Eq[Foo] = Eq.fromUniversalEquals[Foo]
}

sealed abstract class EncodingDecodingExampleData {

  lazy val ints: List[Int] = {
    val random = new Random(100L /* use the same seed for all benchmarks */ )
    Iterator.continually(random.nextInt(10000000)).take(10000).toList
  }

  lazy val foos: Map[String, Foo] = List
    .tabulate(100) { i =>
      ("x" * i) -> Foo("y" * i, (i + 2.0) / (i + 1.0), i, i * 1000L, (0 to i).map(_ % 2 == 0).toList)
    }
    .toMap

  val intsJson: Array[Byte] = {
    import io.circe._
    implicitly[Encoder[List[Int]]].apply(ints).noSpaces.getBytes(UTF_8)
  }

  val foosJson: Array[Byte] = {
    import io.circe._
    import CirceCodecs._
    implicitly[Encoder[Map[String, Foo]]].apply(foos).noSpaces.getBytes(UTF_8)
  }

  val emptyArrayJson: Array[Byte] = "[]".getBytes(UTF_8)
}
