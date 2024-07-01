/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

import io.bullet.borer._
import org.openjdk.jmh.annotations._
import io.bullet.borer.derivation.MapBasedCodecs._

object BorerCodecs {

  implicit val intsEncoder: Encoder[List[Int]] = implicitly[Encoder[List[Int]]]
  implicit val intsDecoder: Decoder[List[Int]] = implicitly[Decoder[List[Int]]]

  object Derived {
    implicit val fooCodec: Codec[Foo]                   = deriveCodec[Foo]
    implicit val foosEncoder: Encoder[Map[String, Foo]] = implicitly[Encoder[Map[String, Foo]]]
    implicit val foosDecoder: Decoder[Map[String, Foo]] = implicitly[Decoder[Map[String, Foo]]]
  }

  object Manual {

    implicit val fooCodec: Codec[Foo] = Codec[Foo](
      encoder = (w, x) => {
        w.writeMapStart()
          .writeString("string")
          .writeString(x.string)
          .writeString("double")
          .writeDouble(x.double)
          .writeString("int")
          .writeInt(x.int)
          .writeString("long")
          .writeLong(x.long)
        x.listOfBools
          .foldLeft(w.writeString("listOfBools").writeArrayStart())(_ writeBoolean _)
          .writeBreak()
          .writeBreak()
      },
      decoder = { r =>
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
    implicit val foosEncoder: Encoder[Map[String, Foo]] = implicitly[Encoder[Map[String, Foo]]]
    implicit val foosDecoder: Decoder[Map[String, Foo]] = implicitly[Decoder[Map[String, Foo]]]
  }
}

import BorerCodecs._

class BorerEncodingBenchmark extends EncodingBenchmark {

  @Benchmark
  def encodeFoosD: Array[Byte] = Json.encode(foos)(Derived.foosEncoder).toByteArray

  @Benchmark
  def encodeFoosM: Array[Byte] = Json.encode(foos)(Manual.foosEncoder).toByteArray

  @Benchmark
  def encodeInts: Array[Byte] = Json.encode(ints).toByteArray

  @Benchmark
  def encodeEmptyArray: Array[Byte] = Json.encode(List.empty[Int]).toByteArray
}

class BorerDecodingBenchmark extends DecodingBenchmark {

  @Benchmark
  def decodeFoosD: Map[String, Foo] = Json.decode(foosJson).to[Map[String, Foo]](Derived.foosDecoder).value

  @Benchmark
  def decodeFoosM: Map[String, Foo] = Json.decode(foosJson).to[Map[String, Foo]](Manual.foosDecoder).value

  @Benchmark
  def decodeInts: List[Int] = Json.decode(intsJson).to[List[Int]].value

  @Benchmark
  def decodeEmptyArray: List[Int] = Json.decode(emptyArrayJson).to[List[Int]].value
}

class BorerDomBenchmark extends DomBenchmark {

  private var root: Dom.Element = _
  def setup(): Unit             = root = Json.decode(fileBytes).to[Dom.Element].value

  @Benchmark
  def encodeDom: Array[Byte] = Json.encode(root).toByteArray

  @Benchmark
  def decodeDom: Dom.Element = Json.decode(fileBytes).to[Dom.Element].value
}

class BorerModelBenchmark extends DomBenchmark {

  private var root: Product  = _
  private val encodingConfig = Json.EncodingConfig(bufferSize = 16384)

  // format: OFF
  implicit lazy val codec: Codec[Product] = {
    val c = fileName match {
      case "australia-abc.json" =>
        implicit val a: Codec[Australia.Geometry] = deriveCodec[Australia.Geometry]
        implicit val b: Codec[Australia.Properties] = deriveCodec[Australia.Properties]
        implicit val c: Codec[Australia.Properties1] = deriveCodec[Australia.Properties1]
        implicit val d: Codec[Australia.Features] = deriveCodec[Australia.Features]
        implicit val e: Codec[Australia.Crs] = deriveCodec[Australia.Crs]
        deriveCodec[Australia.RootInterface]

      case "bitcoin.json" =>
        implicit val a: Codec[Bitcoin.SpendingOutpoints] = deriveCodec[Bitcoin.SpendingOutpoints]
        implicit val b: Codec[Bitcoin.PrevOut] = deriveCodec[Bitcoin.PrevOut]
        implicit val c: Codec[Bitcoin.Inputs] = deriveCodec[Bitcoin.Inputs]
        implicit val d: Codec[Bitcoin.Out] = deriveCodec[Bitcoin.Out]
        implicit val e: Codec[Bitcoin.Txs] = deriveCodec[Bitcoin.Txs]
        deriveCodec[Bitcoin.RootInterface]

      case "doj-blog.json" =>
        implicit val a: Codec[DojBlog.ResponseInfo] = deriveCodec[DojBlog.ResponseInfo]
        implicit val b: Codec[DojBlog.Resultset] = deriveCodec[DojBlog.Resultset]
        implicit val c: Codec[DojBlog.Metadata] = deriveCodec[DojBlog.Metadata]
        implicit val d: Codec[DojBlog.Component] = deriveCodec[DojBlog.Component]
        implicit val e: Codec[DojBlog.Results] = deriveCodec[DojBlog.Results]
        deriveCodec[DojBlog.RootInterface]

      case "eu-lobby-country.json" =>
        implicit val a: Codec[EuLobbyCountry.Facets] = deriveCodec[EuLobbyCountry.Facets]
        implicit val b: Codec[EuLobbyCountry.Results] = deriveCodec[EuLobbyCountry.Results]
        deriveCodec[EuLobbyCountry.RootInterface]

      case "eu-lobby-financial.json" =>
        implicit val a: Codec[EuLobbyFinancial.Facets] = deriveCodec[EuLobbyFinancial.Facets]
        implicit val b: Codec[EuLobbyFinancial.CustomIncomes] = deriveCodec[EuLobbyFinancial.CustomIncomes]
        implicit val c: Codec[EuLobbyFinancial.Results] = deriveCodec[EuLobbyFinancial.Results]
        deriveCodec[EuLobbyFinancial.RootInterface]

      case "eu-lobby-repr.json" =>
        implicit val a: Codec[EuLobbyRepr.Facets] = deriveCodec[EuLobbyRepr.Facets]
        implicit val b: Codec[EuLobbyRepr.Results] = deriveCodec[EuLobbyRepr.Results]
        deriveCodec[EuLobbyRepr.RootInterface]

      case "github-events.json" =>
        import GithubEvents._
        implicit val a: Codec[Actor] = deriveCodec[Actor]
        implicit val b: Codec[Author] = deriveCodec[Author]
        implicit val c: Codec[Self] = deriveCodec[Self]
        implicit val d: Codec[Owner] = deriveCodec[Owner]
        implicit val e: Codec[Org] = deriveCodec[Org]
        implicit val f: Codec[Links] = deriveCodec[Links]
        implicit val g: Codec[Links1] = deriveCodec[Links1]
        implicit val h: Codec[Labels] = deriveCodec[Labels]
        implicit val i: Codec[License] = deriveCodec[License]
        implicit val j: Codec[Repo] = deriveCodec[Repo]
        implicit val k: Codec[Repo1] = deriveCodec[Repo1]
        implicit val l: Codec[Comment] = deriveCodec[Comment]
        implicit val m: Codec[Comment1] = deriveCodec[Comment1]
        implicit val n: Codec[Commits] = deriveCodec[Commits]
        implicit val o: Codec[CreateEvent] = deriveCodec[CreateEvent]
        implicit val p: Codec[Forkee] = deriveCodec[Forkee]
        implicit val q: Codec[ForkEvent[Forkee]] = deriveCodec[ForkEvent[Forkee]]
        implicit val r: Codec[Head[Repo1]] = deriveCodec[Head[Repo1]]
        implicit val s: Codec[PullRequest] = deriveCodec[PullRequest]
        implicit val t: Codec[PullRequest1[Head[Repo1]]] = deriveCodec[PullRequest1[Head[Repo1]]]
        implicit val u: Codec[PullRequest2[Head[Repo1]]] = deriveCodec[PullRequest2[Head[Repo1]]]
        implicit val v: Codec[PullRequestEvent[Head[Repo1]]] = deriveCodec[PullRequestEvent[Head[Repo1]]]
        implicit val w: Codec[PullRequestReviewCommentEvent[Head[Repo1]]] = deriveCodec[PullRequestReviewCommentEvent[Head[Repo1]]]
        implicit val x: Codec[Issue] = deriveCodec[Issue]
        implicit val y: Codec[Issue1] = deriveCodec[Issue1]
        implicit val z: Codec[IssueCommentEvent] = deriveCodec[IssueCommentEvent]
        implicit val A: Codec[IssuesEvent] = deriveCodec[IssuesEvent]
        implicit val B: Codec[PushEvent] = deriveCodec[PushEvent]
        implicit val C: Codec[WatchEvent] = deriveCodec[WatchEvent]
        implicit val D: Codec[RootInterface[Head[Repo1], Forkee]] = deriveCodec[RootInterface[Head[Repo1], Forkee]]
        Codec.of[List[RootInterface[Head[Repo1], Forkee]]]

      case "github-gists.json" =>
        implicit val a: Codec[GithubGists.Owner] = deriveCodec[GithubGists.Owner]
        implicit val b: Codec[GithubGists.FileData] = deriveCodec[GithubGists.FileData]
        implicit val c: Codec[GithubGists.RootInterface] = deriveCodec[GithubGists.RootInterface]
        Codec.of[List[GithubGists.RootInterface]]

      case "json-generator.json" =>
        implicit val a: Codec[JsonGenerator.Friends] = deriveCodec[JsonGenerator.Friends]
        implicit val b: Codec[JsonGenerator.Name] = deriveCodec[JsonGenerator.Name]
        implicit val c: Codec[JsonGenerator.RootInterface] = deriveCodec[JsonGenerator.RootInterface]
        Codec.of[List[JsonGenerator.RootInterface]]

      case "meteorites.json" =>
        implicit val a: Codec[Meteorites.Geolocation] = deriveCodec[Meteorites.Geolocation]
        implicit val b: Codec[Meteorites.RootInterface] = deriveCodec[Meteorites.RootInterface]
        Codec.of[List[Meteorites.RootInterface]]

      case "movies.json" =>
        implicit val a: Codec[Movies.RootInterface] = deriveCodec[Movies.RootInterface]
        Codec.of[List[Movies.RootInterface]]

      case "reddit-scala.json" =>
        implicit val a: Codec[Reddit.Oembed] = deriveCodec[Reddit.Oembed]
        implicit val b: Codec[Reddit.SecureMedia] = deriveCodec[Reddit.SecureMedia]
        implicit val c: Codec[Reddit.MediaEmbed] = deriveCodec[Reddit.MediaEmbed]
        implicit val d: Codec[Reddit.Gildings] = deriveCodec[Reddit.Gildings]
        implicit val e: Codec[Reddit.Data] = deriveCodec[Reddit.Data]
        implicit val f: Codec[Reddit.Child[Reddit.Data]] = deriveCodec[Reddit.Child[Reddit.Data]]
        implicit val g: Codec[Reddit.Data0[Reddit.Data]] = deriveCodec[Reddit.Data0[Reddit.Data]]
        deriveCodec[Reddit.RootInterface[Reddit.Data]]

      case "rick-morty.json" =>
        implicit val a: Codec[RickMorty.Rating] = deriveCodec[RickMorty.Rating]
        implicit val b: Codec[RickMorty.Schedule] = deriveCodec[RickMorty.Schedule]
        implicit val c: Codec[RickMorty.Country] = deriveCodec[RickMorty.Country]
        implicit val d: Codec[RickMorty.Network] = deriveCodec[RickMorty.Network]
        implicit val e: Codec[RickMorty.Image] = deriveCodec[RickMorty.Image]
        implicit val f: Codec[RickMorty.Externals] = deriveCodec[RickMorty.Externals]
        implicit val g: Codec[RickMorty.Self] = deriveCodec[RickMorty.Self]
        implicit val h: Codec[RickMorty.Links] = deriveCodec[RickMorty.Links]
        implicit val i: Codec[RickMorty.Links1] = deriveCodec[RickMorty.Links1]
        implicit val j: Codec[RickMorty.Episodes] = deriveCodec[RickMorty.Episodes]
        implicit val k: Codec[RickMorty.Embedded] = deriveCodec[RickMorty.Embedded]
        deriveCodec[RickMorty.RootInterface]

      case "temp-anomaly.json" =>
        implicit val a: Codec[TempAnomaly.Description] = deriveCodec[TempAnomaly.Description]
        deriveCodec[TempAnomaly.RootInterface]

      case "thai-cinemas.json" =>
        implicit val a: Codec[ThaiCinemas.Group] = deriveCodec[ThaiCinemas.Group]
        implicit val b: Codec[ThaiCinemas.Results] = deriveCodec[ThaiCinemas.Results]
        deriveCodec[ThaiCinemas.RootInterface]

      case "turkish.json" =>
        implicit val a: Codec[Turkish.Event] = deriveCodec[Turkish.Event]
        implicit val b: Codec[Turkish.Result] = deriveCodec[Turkish.Result]
        deriveCodec[Turkish.RootInterface]

      case "twitter_api_compact_response.json" | "twitter_api_response.json" =>
        implicit val a: Codec[TwitterApiResponse.Urls] = deriveCodec[TwitterApiResponse.Urls]
        implicit val b: Codec[TwitterApiResponse.Url] = deriveCodec[TwitterApiResponse.Url]
        implicit val c: Codec[TwitterApiResponse.UserMentions] = deriveCodec[TwitterApiResponse.UserMentions]
        implicit val d: Codec[TwitterApiResponse.Entities] = deriveCodec[TwitterApiResponse.Entities]
        implicit val e: Codec[TwitterApiResponse.Entities1] = deriveCodec[TwitterApiResponse.Entities1]
        implicit val f: Codec[TwitterApiResponse.User] = deriveCodec[TwitterApiResponse.User]
        implicit val g: Codec[TwitterApiResponse.RetweetedStatus] = deriveCodec[TwitterApiResponse.RetweetedStatus]
        implicit val h: Codec[TwitterApiResponse.RootInterface] = deriveCodec[TwitterApiResponse.RootInterface]
        Codec.of[List[TwitterApiResponse.RootInterface]]
    }
    c.asInstanceOf[Codec[Product]]
  }
  // format: ON

  def setup(): Unit = root = Json.decode(fileBytes).to[Product].value

  @Benchmark
  def encodeModel: Array[Byte] = Json.encode(root).withConfig(encodingConfig).toByteArray

  @Benchmark
  def decodeModel: Product = Json.decode(fileBytes).to[Product].value
}
