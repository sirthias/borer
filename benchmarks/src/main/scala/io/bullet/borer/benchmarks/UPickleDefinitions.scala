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

  implicit val foosWriter: Writer[Map[String, Foo]] = implicitly[Writer[Map[String, Foo]]]
  implicit val intsWriter: Writer[List[Int]]        = implicitly[Writer[List[Int]]]

  implicit val foosReader: Reader[Map[String, Foo]] = implicitly[Reader[Map[String, Foo]]]
  implicit val intsReader: Reader[List[Int]]        = implicitly[Reader[List[Int]]]
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
        implicit val a: ReadWriter[Australia.Geometry] = macroRW[Australia.Geometry]
        implicit val b: ReadWriter[Australia.Properties] = macroRW[Australia.Properties]
        implicit val c: ReadWriter[Australia.Properties1] = macroRW[Australia.Properties1]
        implicit val d: ReadWriter[Australia.Features] = macroRW[Australia.Features]
        implicit val e: ReadWriter[Australia.Crs] = macroRW[Australia.Crs]
        macroRW[Australia.RootInterface]

      case "bitcoin.json" =>
        implicit val a: ReadWriter[Bitcoin.SpendingOutpoints] = macroRW[Bitcoin.SpendingOutpoints]
        implicit val b: ReadWriter[Bitcoin.PrevOut] = macroRW[Bitcoin.PrevOut]
        implicit val c: ReadWriter[Bitcoin.Inputs] = macroRW[Bitcoin.Inputs]
        implicit val d: ReadWriter[Bitcoin.Out] = macroRW[Bitcoin.Out]
        implicit val e: ReadWriter[Bitcoin.Txs] = macroRW[Bitcoin.Txs]
        macroRW[Bitcoin.RootInterface]

      case "doj-blog.json" =>
        implicit val a: ReadWriter[DojBlog.ResponseInfo] = macroRW[DojBlog.ResponseInfo]
        implicit val b: ReadWriter[DojBlog.Resultset] = macroRW[DojBlog.Resultset]
        implicit val c: ReadWriter[DojBlog.Metadata] = macroRW[DojBlog.Metadata]
        implicit val d: ReadWriter[DojBlog.Component] = macroRW[DojBlog.Component]
        implicit val e: ReadWriter[DojBlog.Results] = macroRW[DojBlog.Results]
        macroRW[DojBlog.RootInterface]

      case "eu-lobby-country.json" =>
        implicit val a: ReadWriter[EuLobbyCountry.Facets] = macroRW[EuLobbyCountry.Facets]
        implicit val b: ReadWriter[EuLobbyCountry.Results] = macroRW[EuLobbyCountry.Results]
        macroRW[EuLobbyCountry.RootInterface]

      case "eu-lobby-financial.json" =>
        implicit val a: ReadWriter[EuLobbyFinancial.Facets] = macroRW[EuLobbyFinancial.Facets]
        implicit val b: ReadWriter[EuLobbyFinancial.CustomIncomes] = macroRW[EuLobbyFinancial.CustomIncomes]
        implicit val c: ReadWriter[EuLobbyFinancial.Results] = macroRW[EuLobbyFinancial.Results]
        macroRW[EuLobbyFinancial.RootInterface]

      case "eu-lobby-repr.json" =>
        implicit val a: ReadWriter[EuLobbyRepr.Facets] = macroRW[EuLobbyRepr.Facets]
        implicit val b: ReadWriter[EuLobbyRepr.Results] = macroRW[EuLobbyRepr.Results]
        macroRW[EuLobbyRepr.RootInterface]

      case "github-events.json" =>
        import GithubEvents._
        implicit val _a: ReadWriter[Actor] = macroRW[Actor]
        implicit val _b: ReadWriter[Author] = macroRW[Author]
        implicit val _c: ReadWriter[Self] = macroRW[Self]
        implicit val _d: ReadWriter[Owner] = macroRW[Owner]
        implicit val _e: ReadWriter[Org] = macroRW[Org]
        implicit val _f: ReadWriter[Links] = macroRW[Links]
        implicit val _g: ReadWriter[Links1] = macroRW[Links1]
        implicit val _h: ReadWriter[Labels] = macroRW[Labels]
        implicit val _i: ReadWriter[License] = macroRW[License]
        implicit val _j: ReadWriter[Repo] = macroRW[Repo]
        implicit val _k: ReadWriter[Repo1_64] = macroRW[Repo1_64]
        implicit val _l: ReadWriter[Comment] = macroRW[Comment]
        implicit val _m: ReadWriter[Comment1] = macroRW[Comment1]
        implicit val _n: ReadWriter[Commits] = macroRW[Commits]
        implicit val _o: ReadWriter[CreateEvent] = macroRW[CreateEvent]
        implicit val _p: ReadWriter[Forkee_64] = macroRW[Forkee_64]
        implicit val _q: ReadWriter[ForkEvent[Forkee_64]] = macroRW[ForkEvent[Forkee_64]]
        implicit val _r: ReadWriter[Head[Repo1_64]] = macroRW[Head[Repo1_64]]
        implicit val _s: ReadWriter[PullRequest] = macroRW[PullRequest]
        implicit val _t: ReadWriter[PullRequest1[Head[Repo1_64]]] = macroRW[PullRequest1[Head[Repo1_64]]]
        implicit val _u: ReadWriter[PullRequest2[Head[Repo1_64]]] = macroRW[PullRequest2[Head[Repo1_64]]]
        implicit val _v: ReadWriter[PullRequestEvent[Head[Repo1_64]]] = macroRW[PullRequestEvent[Head[Repo1_64]]]
        implicit val _w: ReadWriter[PullRequestReviewCommentEvent[Head[Repo1_64]]] = macroRW[PullRequestReviewCommentEvent[Head[Repo1_64]]]
        implicit val _x: ReadWriter[Issue] = macroRW[Issue]
        implicit val _y: ReadWriter[Issue1] = macroRW[Issue1]
        implicit val _z: ReadWriter[IssueCommentEvent] = macroRW[IssueCommentEvent]
        implicit val _A: ReadWriter[IssuesEvent] = macroRW[IssuesEvent]
        implicit val _B: ReadWriter[PushEvent] = macroRW[PushEvent]
        implicit val _C: ReadWriter[WatchEvent] = macroRW[WatchEvent]
        implicit val _D: ReadWriter[RootInterface[Head[Repo1_64], Forkee_64]] = macroRW[RootInterface[Head[Repo1_64], Forkee_64]]
        ReadWriter.join[List[RootInterface[Head[Repo1_64], Forkee_64]]](SeqLikeReader, SeqLikeWriter)

      case "github-gists.json" =>
        implicit val a: ReadWriter[GithubGists.Owner] = macroRW[GithubGists.Owner]
        implicit val b: ReadWriter[GithubGists.FileData] = macroRW[GithubGists.FileData]
        implicit val c: ReadWriter[GithubGists.RootInterface] = macroRW[GithubGists.RootInterface]
        ReadWriter.join[List[GithubGists.RootInterface]](SeqLikeReader, SeqLikeWriter)

      case "json-generator.json" =>
        implicit val a: ReadWriter[JsonGenerator.Friends] = macroRW[JsonGenerator.Friends]
        implicit val b: ReadWriter[JsonGenerator.Name] = macroRW[JsonGenerator.Name]
        implicit val c: ReadWriter[JsonGenerator.RootInterface] = macroRW[JsonGenerator.RootInterface]
        ReadWriter.join[List[JsonGenerator.RootInterface]](SeqLikeReader, SeqLikeWriter)

      case "meteorites.json" =>
        implicit val a: ReadWriter[Meteorites.Geolocation] = macroRW[Meteorites.Geolocation]
        implicit val b: ReadWriter[Meteorites.RootInterface] = macroRW[Meteorites.RootInterface]
        ReadWriter.join[List[Meteorites.RootInterface]](SeqLikeReader, SeqLikeWriter)

      case "movies.json" =>
        implicit val a: ReadWriter[Movies.RootInterface] = macroRW[Movies.RootInterface]
        ReadWriter.join[List[Movies.RootInterface]](SeqLikeReader, SeqLikeWriter)

      case "reddit-scala.json" =>
        implicit val a: ReadWriter[Reddit.Oembed] = macroRW[Reddit.Oembed]
        implicit val b: ReadWriter[Reddit.SecureMedia] = macroRW[Reddit.SecureMedia]
        implicit val c: ReadWriter[Reddit.MediaEmbed] = macroRW[Reddit.MediaEmbed]
        implicit val d: ReadWriter[Reddit.Gildings] = macroRW[Reddit.Gildings]
        implicit val e: ReadWriter[Reddit.Data_64] = macroRW[Reddit.Data_64]
        implicit val f: ReadWriter[Reddit.Child[Reddit.Data_64]] = macroRW[Reddit.Child[Reddit.Data_64]]
        implicit val g: ReadWriter[Reddit.Data0[Reddit.Data_64]] = macroRW[Reddit.Data0[Reddit.Data_64]]
        macroRW[Reddit.RootInterface[Reddit.Data_64]]

      case "rick-morty.json" =>
        implicit val a: ReadWriter[RickMorty.Rating] = macroRW[RickMorty.Rating]
        implicit val b: ReadWriter[RickMorty.Schedule] = macroRW[RickMorty.Schedule]
        implicit val c: ReadWriter[RickMorty.Country] = macroRW[RickMorty.Country]
        implicit val d: ReadWriter[RickMorty.Network] = macroRW[RickMorty.Network]
        implicit val e: ReadWriter[RickMorty.Image] = macroRW[RickMorty.Image]
        implicit val f: ReadWriter[RickMorty.Externals] = macroRW[RickMorty.Externals]
        implicit val g: ReadWriter[RickMorty.Self] = macroRW[RickMorty.Self]
        implicit val h: ReadWriter[RickMorty.Links] = macroRW[RickMorty.Links]
        implicit val i: ReadWriter[RickMorty.Links1] = macroRW[RickMorty.Links1]
        implicit val j: ReadWriter[RickMorty.Episodes] = macroRW[RickMorty.Episodes]
        implicit val k: ReadWriter[RickMorty.Embedded] = macroRW[RickMorty.Embedded]
        macroRW[RickMorty.RootInterface]

      case "temp-anomaly.json" =>
        implicit val a: ReadWriter[TempAnomaly.Description] = macroRW[TempAnomaly.Description]
        macroRW[TempAnomaly.RootInterface]

      case "thai-cinemas.json" =>
        implicit val a: ReadWriter[ThaiCinemas.Group] = macroRW[ThaiCinemas.Group]
        implicit val b: ReadWriter[ThaiCinemas.Results] = macroRW[ThaiCinemas.Results]
        macroRW[ThaiCinemas.RootInterface]

      case "turkish.json" =>
        implicit val a: ReadWriter[Turkish.Event] = macroRW[Turkish.Event]
        implicit val b: ReadWriter[Turkish.Result] = macroRW[Turkish.Result]
        macroRW[Turkish.RootInterface]

      case "twitter_api_compact_response.json" | "twitter_api_response.json" =>
        implicit val a: ReadWriter[TwitterApiResponse.Urls] = macroRW[TwitterApiResponse.Urls]
        implicit val b: ReadWriter[TwitterApiResponse.Url] = macroRW[TwitterApiResponse.Url]
        implicit val c: ReadWriter[TwitterApiResponse.UserMentions] = macroRW[TwitterApiResponse.UserMentions]
        implicit val d: ReadWriter[TwitterApiResponse.Entities] = macroRW[TwitterApiResponse.Entities]
        implicit val e: ReadWriter[TwitterApiResponse.Entities1] = macroRW[TwitterApiResponse.Entities1]
        implicit val f: ReadWriter[TwitterApiResponse.User] = macroRW[TwitterApiResponse.User]
        implicit val g: ReadWriter[TwitterApiResponse.RetweetedStatus] = macroRW[TwitterApiResponse.RetweetedStatus]
        implicit val h: ReadWriter[TwitterApiResponse.RootInterface] = macroRW[TwitterApiResponse.RootInterface]
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
