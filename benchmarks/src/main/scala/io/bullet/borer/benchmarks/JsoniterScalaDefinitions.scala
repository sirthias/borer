/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import io.bullet.borer.{Default, Nullable}
import org.openjdk.jmh.annotations.*

import scala.compiletime.uninitialized

object JsoniterScalaCodecs:

  val foosCodec: JsonValueCodec[Map[String, Foo]] =
    JsonCodecMaker.make[Map[String, Foo]](CodecMakerConfig)

  val intsCodec: JsonValueCodec[List[Int]] =
    JsonCodecMaker.make[List[Int]](CodecMakerConfig)

object JsoniterScalaConfig:
  val writerConfig: WriterConfig = WriterConfig.withPreferredBufSize(4 * 1024 * 1024 /* 4Mb */ )

import io.bullet.borer.benchmarks.JsoniterScalaCodecs.*
import io.bullet.borer.benchmarks.JsoniterScalaConfig.*

class JsoniterScalaEncodingBenchmark extends EncodingBenchmark:

  @Benchmark
  def encodeFoos: Array[Byte] = writeToArray(foos, writerConfig)(using foosCodec)

  @Benchmark
  def encodeInts: Array[Byte] = writeToArray(ints, writerConfig)(using intsCodec)

  @Benchmark
  def encodeEmptyArray: Array[Byte] = writeToArray(List.empty[Int])(using intsCodec)

class JsoniterScalaDecodingBenchmark extends DecodingBenchmark:

  @Benchmark
  def decodeFoos: Map[String, Foo] = readFromArray(foosJson)(using foosCodec)

  @Benchmark
  def decodeInts: List[Int] = readFromArray(intsJson)(using intsCodec)

  @Benchmark
  def decodeEmptyArray: List[Int] = readFromArray(emptyArrayJson)(using intsCodec)

class JsoniterScalaModelBenchmark extends DomBenchmark:

  private var root: Product = uninitialized

  private var codec: JsonValueCodec[Product] = uninitialized

  given nullableDoubleCodec: JsonValueCodec[Nullable[Double]] with
    override val nullValue: Nullable[Double]                             = Nullable(Default.of[Double])
    override def encodeValue(x: Nullable[Double], out: JsonWriter): Unit = out.writeVal(x: Double)

    override def decodeValue(in: JsonReader, default: Nullable[Double]): Nullable[Double] =
      if (in.isNextToken('n')) in.readNullOrError(default, "expected Double or Null")
      else
        in.rollbackToken()
        Nullable(in.readDouble())

  given nullableIntCodec: JsonValueCodec[Nullable[Int]] with
    override val nullValue: Nullable[Int]                             = Nullable(Default.of[Int])
    override def encodeValue(x: Nullable[Int], out: JsonWriter): Unit = out.writeVal(x: Int)

    override def decodeValue(in: JsonReader, default: Nullable[Int]): Nullable[Int] =
      if (in.isNextToken('n')) in.readNullOrError(default, "expected Int or Null")
      else
        in.rollbackToken()
        Nullable(in.readInt())

  given nullableStringCodec: JsonValueCodec[Nullable[String]] with
    override val nullValue: Nullable[String] = Nullable(Default.of[String])

    override def encodeValue(x: Nullable[String], out: JsonWriter): Unit =
      if (x == null) out.writeNull() else out.writeVal(x: String)

    override def decodeValue(in: JsonReader, default: Nullable[String]): Nullable[String] =
      if (in.isNextToken('n')) in.readNullOrError(default, "expected String or Null")
      else
        in.rollbackToken()
        Nullable(in.readString(null))

  private def nullableOptionCodec[T](codec: JsonValueCodec[T]): JsonValueCodec[Nullable[Option[T]]] =
    new JsonValueCodec[Nullable[Option[T]]]:
      override val nullValue: Nullable[Option[T]] = Nullable(None)

      override def encodeValue(x: Nullable[Option[T]], out: JsonWriter): Unit =
        (x: Option[T]) match
          case Some(value) => codec.encodeValue(value, out)
          case None        => out.writeNull()

      override def decodeValue(in: JsonReader, default: Nullable[Option[T]]): Nullable[Option[T]] =
        if (in.isNextToken('n')) in.readNullOrError(default, "expected value or Null")
        else
          in.rollbackToken()
          Nullable(Some(codec.decodeValue(in, codec.nullValue)))

  def setup(): Unit =
    codec = (
      fileName match
        case "australia-abc.json"      => JsonCodecMaker.make[Australia.RootInterface](CodecMakerConfig)
        case "bitcoin.json"            => JsonCodecMaker.make[Bitcoin.RootInterface](CodecMakerConfig)
        case "doj-blog.json"           => JsonCodecMaker.make[DojBlog.RootInterface](CodecMakerConfig)
        case "eu-lobby-country.json"   => JsonCodecMaker.make[EuLobbyCountry.RootInterface](CodecMakerConfig)
        case "eu-lobby-financial.json" => JsonCodecMaker.make[EuLobbyFinancial.RootInterface](CodecMakerConfig)
        case "eu-lobby-repr.json"      => JsonCodecMaker.make[EuLobbyRepr.RootInterface](CodecMakerConfig)
        case "github-events.json"      =>
          JsonCodecMaker
            .make[List[GithubEvents.RootInterface[GithubEvents.Head[GithubEvents.Repo1], GithubEvents.Forkee]]](
              CodecMakerConfig)
        case "github-gists.json"   => JsonCodecMaker.make[List[GithubGists.RootInterface]](CodecMakerConfig)
        case "json-generator.json" => JsonCodecMaker.make[List[JsonGenerator.RootInterface]](CodecMakerConfig)
        case "meteorites.json"     => JsonCodecMaker.make[List[Meteorites.RootInterface]](CodecMakerConfig)
        case "movies.json"         => JsonCodecMaker.make[List[Movies.RootInterface]](CodecMakerConfig)
        case "reddit-scala.json"   =>
          given secureMediaCodec: JsonValueCodec[Nullable[Option[Reddit.SecureMedia]]] =
            nullableOptionCodec(JsonCodecMaker.make[Reddit.SecureMedia](CodecMakerConfig))
          given mediaEmbedCodec: JsonValueCodec[Nullable[Option[Reddit.MediaEmbed]]] =
            nullableOptionCodec(JsonCodecMaker.make[Reddit.MediaEmbed](CodecMakerConfig))
          JsonCodecMaker.make[Reddit.RootInterface[Reddit.Data]](CodecMakerConfig)
        case "rick-morty.json"   => JsonCodecMaker.make[RickMorty.RootInterface](CodecMakerConfig)
        case "temp-anomaly.json" => JsonCodecMaker.make[TempAnomaly.RootInterface](CodecMakerConfig)
        case "thai-cinemas.json" => JsonCodecMaker.make[ThaiCinemas.RootInterface](CodecMakerConfig)
        case "turkish.json"      => JsonCodecMaker.make[Turkish.RootInterface](CodecMakerConfig)
        case "twitter_api_compact_response.json" | "twitter_api_response.json" =>
          JsonCodecMaker.make[List[TwitterApiResponse.RootInterface]](CodecMakerConfig)
    ).asInstanceOf[JsonValueCodec[Product]]
    root = readFromArray(fileBytes)(using codec)

  @Benchmark
  def encodeModel: Array[Byte] = writeToArray(root, writerConfig)(using codec)

  @Benchmark
  def decodeModel: Product = readFromArray(fileBytes)(using codec)
