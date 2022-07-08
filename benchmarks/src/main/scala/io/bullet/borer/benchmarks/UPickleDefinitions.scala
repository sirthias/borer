/*
 * Copyright (c) 2019-2022 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

import java.nio.charset.StandardCharsets.UTF_8
import io.bullet.borer.{Default, Nullable}
import org.openjdk.jmh.annotations._
import upickle.default._

object UPickleCodecs {
  implicit val fooRW: ReadWriter[Foo] = macroRW

  implicit val foosWriter = implicitly[Writer[Map[String, Foo]]]
  implicit val intsWriter = implicitly[Writer[List[Int]]]

  implicit val foosReader = implicitly[Reader[Map[String, Foo]]]
  implicit val intsReader = implicitly[Reader[List[Int]]]
}

import io.bullet.borer.benchmarks.UPickleCodecs._

class UPickleEncodingBenchmark extends EncodingBenchmark {

  @Benchmark
  def encodeFoos: Array[Byte] = write(foos).getBytes(UTF_8)

  @Benchmark
  def encodeInts: Array[Byte] = write(ints).getBytes(UTF_8)

  @Benchmark
  def encodeEmptyArray: Array[Byte] = write(List.empty[Int]).getBytes(UTF_8)
}

class UPickleDecodingBenchmark extends DecodingBenchmark {

  @Benchmark
  def decodeFoos: Map[String, Foo] = read[Map[String, Foo]](new String(foosJson, UTF_8))

  @Benchmark
  def decodeInts: List[Int] = read[List[Int]](new String(intsJson, UTF_8))

  @Benchmark
  def decodeEmptyArray: List[Int] = read[List[Int]](new String(emptyArrayJson, UTF_8))
}

class UPickleDomBenchmark extends DomBenchmark {

  private var root: ujson.Value = _
  def setup(): Unit             = root = read[ujson.Value](new String(fileBytes, UTF_8))

  @Benchmark
  def encodeDom: Array[Byte] = write(root).getBytes(UTF_8)

  @Benchmark
  def decodeDom: ujson.Value = read[ujson.Value](new String(fileBytes, UTF_8))
}

//@nowarn("cat=other-match-analysis")
class UPickleModelBenchmark extends DomBenchmark {

  private var root: Product = _

  implicit def nullableWriter[T: Writer]: Writer[Nullable[T]] = implicitly[Writer[T]].comap(_.value)

  implicit def nullableReader[T: Reader: Default]: Reader[Nullable[T]] =
    implicitly[Reader[T]].map(x => Nullable(if (x == null) Default.of[T] else x))

  implicit def OptionWriter[T: Writer]: Writer[Option[T]] =
    implicitly[Writer[T]].comap[Option[T]] {
      case None    => null.asInstanceOf[T]
      case Some(x) => x
    }

  implicit def OptionReader[T: Reader]: Reader[Option[T]] =
    implicitly[Reader[T]].mapNulls {
      case null => None
      case x    => Some(x)
    }

  // format: OFF
  implicit lazy val codec: ReadWriter[Product] = {
    val c = fileName match {
      case "australia-abc.json" =>
        implicit val a = macroRW[Australia.Geometry]
        implicit val b = macroRW[Australia.Properties]
        implicit val c = macroRW[Australia.Properties1]
        implicit val d = macroRW[Australia.Features]
        implicit val e = macroRW[Australia.Crs]
        macroRW[Australia.RootInterface]

      case "bitcoin.json" =>
        implicit val a = macroRW[Bitcoin.SpendingOutpoints]
        implicit val b = macroRW[Bitcoin.PrevOut]
        implicit val c = macroRW[Bitcoin.Inputs]
        implicit val d = macroRW[Bitcoin.Out]
        implicit val e = macroRW[Bitcoin.Txs]
        macroRW[Bitcoin.RootInterface]

      case "doj-blog.json" =>
        implicit val a = macroRW[DojBlog.ResponseInfo]
        implicit val b = macroRW[DojBlog.Resultset]
        implicit val c = macroRW[DojBlog.Metadata]
        implicit val d = macroRW[DojBlog.Component]
        implicit val e = macroRW[DojBlog.Results]
        macroRW[DojBlog.RootInterface]

      case "eu-lobby-country.json" =>
        implicit val a = macroRW[EuLobbyCountry.Facets]
        implicit val b = macroRW[EuLobbyCountry.Results]
        macroRW[EuLobbyCountry.RootInterface]

      case "eu-lobby-financial.json" =>
        implicit val a = macroRW[EuLobbyFinancial.Facets]
        implicit val b = macroRW[EuLobbyFinancial.CustomIncomes]
        implicit val c = macroRW[EuLobbyFinancial.Results]
        macroRW[EuLobbyFinancial.RootInterface]

      case "eu-lobby-repr.json" =>
        implicit val a = macroRW[EuLobbyRepr.Facets]
        implicit val b = macroRW[EuLobbyRepr.Results]
        macroRW[EuLobbyRepr.RootInterface]

      case "github-events.json" =>
        import GithubEvents._
        implicit val _a = macroRW[Actor]
        implicit val _b = macroRW[Author]
        implicit val _c = macroRW[Self]
        implicit val _d = macroRW[Owner]
        implicit val _e = macroRW[Org]
        implicit val _f = macroRW[Links]
        implicit val _g = macroRW[Links1]
        implicit val _h = macroRW[Labels]
        implicit val _i = macroRW[License]
        implicit val _j = macroRW[Repo]
        implicit val _k = macroRW[Repo1_64]
        implicit val _l = macroRW[Comment]
        implicit val _m = macroRW[Comment1]
        implicit val _n = macroRW[Commits]
        implicit val _o = macroRW[CreateEvent]
        implicit val _p = macroRW[Forkee_64]
        implicit val _q = macroRW[ForkEvent[Forkee_64]]
        implicit val _r = macroRW[Head[Repo1_64]]
        implicit val _s = macroRW[PullRequest]
        implicit val _t = macroRW[PullRequest1[Head[Repo1_64]]]
        implicit val _u = macroRW[PullRequest2[Head[Repo1_64]]]
        implicit val _v = macroRW[PullRequestEvent[Head[Repo1_64]]]
        implicit val _w = macroRW[PullRequestReviewCommentEvent[Head[Repo1_64]]]
        implicit val _x = macroRW[Issue]
        implicit val _y = macroRW[Issue1]
        implicit val _z = macroRW[IssueCommentEvent]
        implicit val _A = macroRW[IssuesEvent]
        implicit val _B = macroRW[PushEvent]
        implicit val _C = macroRW[WatchEvent]
        implicit val _D = macroRW[RootInterface[Head[Repo1_64], Forkee_64]]
        ReadWriter.join[List[RootInterface[Head[Repo1_64], Forkee_64]]](SeqLikeReader, SeqLikeWriter)

      case "github-gists.json" =>
        implicit val a = macroRW[GithubGists.Owner]
        implicit val b = macroRW[GithubGists.FileData]
        implicit val c = macroRW[GithubGists.RootInterface]
        ReadWriter.join[List[GithubGists.RootInterface]](SeqLikeReader, SeqLikeWriter)

      case "json-generator.json" =>
        implicit val a = macroRW[JsonGenerator.Friends]
        implicit val b = macroRW[JsonGenerator.Name]
        implicit val c = macroRW[JsonGenerator.RootInterface]
        ReadWriter.join[List[JsonGenerator.RootInterface]](SeqLikeReader, SeqLikeWriter)

      case "meteorites.json" =>
        implicit val a = macroRW[Meteorites.Geolocation]
        implicit val b = macroRW[Meteorites.RootInterface]
        ReadWriter.join[List[Meteorites.RootInterface]](SeqLikeReader, SeqLikeWriter)

      case "movies.json" =>
        implicit val a = macroRW[Movies.RootInterface]
        ReadWriter.join[List[Movies.RootInterface]](SeqLikeReader, SeqLikeWriter)

      case "reddit-scala.json" =>
        implicit val a = macroRW[Reddit.Oembed]
        implicit val b = macroRW[Reddit.SecureMedia]
        implicit val c = macroRW[Reddit.MediaEmbed]
        implicit val d = macroRW[Reddit.Gildings]
        implicit val e = macroRW[Reddit.Data_64]
        implicit val f = macroRW[Reddit.Child[Reddit.Data_64]]
        implicit val g = macroRW[Reddit.Data0[Reddit.Data_64]]
        macroRW[Reddit.RootInterface[Reddit.Data_64]]

      case "rick-morty.json" =>
        implicit val a = macroRW[RickMorty.Rating]
        implicit val b = macroRW[RickMorty.Schedule]
        implicit val c = macroRW[RickMorty.Country]
        implicit val d = macroRW[RickMorty.Network]
        implicit val e = macroRW[RickMorty.Image]
        implicit val f = macroRW[RickMorty.Externals]
        implicit val g = macroRW[RickMorty.Self]
        implicit val h = macroRW[RickMorty.Links]
        implicit val i = macroRW[RickMorty.Links1]
        implicit val j = macroRW[RickMorty.Episodes]
        implicit val k = macroRW[RickMorty.Embedded]
        macroRW[RickMorty.RootInterface]

      case "temp-anomaly.json" =>
        implicit val a = macroRW[TempAnomaly.Description]
        macroRW[TempAnomaly.RootInterface]

      case "thai-cinemas.json" =>
        implicit val a = macroRW[ThaiCinemas.Group]
        implicit val b = macroRW[ThaiCinemas.Results]
        macroRW[ThaiCinemas.RootInterface]

      case "turkish.json" =>
        implicit val a = macroRW[Turkish.Event]
        implicit val b = macroRW[Turkish.Result]
        macroRW[Turkish.RootInterface]

      case "twitter_api_compact_response.json" | "twitter_api_response.json" =>
        implicit val a = macroRW[TwitterApiResponse.Urls]
        implicit val b = macroRW[TwitterApiResponse.Url]
        implicit val c = macroRW[TwitterApiResponse.UserMentions]
        implicit val d = macroRW[TwitterApiResponse.Entities]
        implicit val e = macroRW[TwitterApiResponse.Entities1]
        implicit val f = macroRW[TwitterApiResponse.User]
        implicit val g = macroRW[TwitterApiResponse.RetweetedStatus]
        implicit val h = macroRW[TwitterApiResponse.RootInterface]
        ReadWriter.join[List[TwitterApiResponse.RootInterface]](SeqLikeReader, SeqLikeWriter)
    }
    c.asInstanceOf[ReadWriter[Product]]
  }
  // format: ON

  def setup(): Unit = root = read[Product](fileBytes)

  @Benchmark
  def encodeModel: Array[Byte] = write(root).getBytes(UTF_8)

  @Benchmark
  def decodeModel: Product = read[Product](fileBytes)
}
