/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.benchmarks

import io.bullet.borer.*
import io.bullet.borer.derivation.MapBasedCodecs.*
import org.openjdk.jmh.annotations.*

import scala.compiletime.uninitialized

object BorerCodecs:

  val intsEncoder: Encoder[List[Int]] = summon[Encoder[List[Int]]]
  val intsDecoder: Decoder[List[Int]] = summon[Decoder[List[Int]]]

  object Derived:
    given Codec[Foo] = deriveCodec[Foo]

    val foosEncoder: Encoder[Map[String, Foo]] = summon[Encoder[Map[String, Foo]]]
    val foosDecoder: Decoder[Map[String, Foo]] = summon[Decoder[Map[String, Foo]]]

  object Manual:

    given Codec[Foo] = Codec[Foo](
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
          .foldLeft(w.writeString("listOfBools").writeArrayStart())(_.writeBoolean(_))
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

    val foosEncoder: Encoder[Map[String, Foo]] = summon[Encoder[Map[String, Foo]]]
    val foosDecoder: Decoder[Map[String, Foo]] = summon[Decoder[Map[String, Foo]]]

import BorerCodecs.*

class BorerEncodingBenchmark extends EncodingBenchmark:

  @Benchmark
  def encodeFoosD: Array[Byte] = Json.encode(foos)(using Derived.foosEncoder).toByteArray

  @Benchmark
  def encodeFoosM: Array[Byte] = Json.encode(foos)(using Manual.foosEncoder).toByteArray

  @Benchmark
  def encodeInts: Array[Byte] = Json.encode(ints)(using intsEncoder).toByteArray

  @Benchmark
  def encodeEmptyArray: Array[Byte] = Json.encode(List.empty[Int])(using intsEncoder).toByteArray

class BorerDecodingBenchmark extends DecodingBenchmark:

  @Benchmark
  def decodeFoosD: Map[String, Foo] = Json.decode(foosJson).to[Map[String, Foo]](using Derived.foosDecoder).value

  @Benchmark
  def decodeFoosM: Map[String, Foo] = Json.decode(foosJson).to[Map[String, Foo]](using Manual.foosDecoder).value

  @Benchmark
  def decodeInts: List[Int] = Json.decode(intsJson).to[List[Int]](using intsDecoder).value

  @Benchmark
  def decodeEmptyArray: List[Int] = Json.decode(emptyArrayJson).to[List[Int]](using intsDecoder).value

class BorerDomBenchmark extends DomBenchmark:

  private var root: Dom.Element = uninitialized
  def setup(): Unit             = root = Json.decode(fileBytes).to[Dom.Element].value

  @Benchmark
  def encodeDom: Array[Byte] = Json.encode(root).toByteArray

  @Benchmark
  def decodeDom: Dom.Element = Json.decode(fileBytes).to[Dom.Element].value

class BorerModelBenchmark extends DomBenchmark:

  private var root: Product  = uninitialized
  private val encodingConfig = Json.EncodingConfig(bufferSize = 16384)

  // format: OFF
  private lazy val codec: Codec[Product] =
    val c = fileName match
      case "australia-abc.json" =>
        given Codec[Australia.Geometry] = deriveCodec[Australia.Geometry]
        given Codec[Australia.Properties] = deriveCodec[Australia.Properties]
        given Codec[Australia.Properties1] = deriveCodec[Australia.Properties1]
        given Codec[Australia.Features] = deriveCodec[Australia.Features]
        given Codec[Australia.Crs] = deriveCodec[Australia.Crs]
        deriveCodec[Australia.RootInterface]

      case "bitcoin.json" =>
        given Codec[Bitcoin.SpendingOutpoints] = deriveCodec[Bitcoin.SpendingOutpoints]
        given Codec[Bitcoin.PrevOut] = deriveCodec[Bitcoin.PrevOut]
        given Codec[Bitcoin.Inputs] = deriveCodec[Bitcoin.Inputs]
        given Codec[Bitcoin.Out] = deriveCodec[Bitcoin.Out]
        given Codec[Bitcoin.Txs] = deriveCodec[Bitcoin.Txs]
        deriveCodec[Bitcoin.RootInterface]

      case "doj-blog.json" =>
        given Codec[DojBlog.ResponseInfo] = deriveCodec[DojBlog.ResponseInfo]
        given Codec[DojBlog.Resultset] = deriveCodec[DojBlog.Resultset]
        given Codec[DojBlog.Metadata] = deriveCodec[DojBlog.Metadata]
        given Codec[DojBlog.Component] = deriveCodec[DojBlog.Component]
        given Codec[DojBlog.Results] = deriveCodec[DojBlog.Results]
        deriveCodec[DojBlog.RootInterface]

      case "eu-lobby-country.json" =>
        given Codec[EuLobbyCountry.Facets] = deriveCodec[EuLobbyCountry.Facets]
        given Codec[EuLobbyCountry.Results] = deriveCodec[EuLobbyCountry.Results]
        deriveCodec[EuLobbyCountry.RootInterface]

      case "eu-lobby-financial.json" =>
        given Codec[EuLobbyFinancial.Facets] = deriveCodec[EuLobbyFinancial.Facets]
        given Codec[EuLobbyFinancial.CustomIncomes] = deriveCodec[EuLobbyFinancial.CustomIncomes]
        given Codec[EuLobbyFinancial.Results] = deriveCodec[EuLobbyFinancial.Results]
        deriveCodec[EuLobbyFinancial.RootInterface]

      case "eu-lobby-repr.json" =>
        given Codec[EuLobbyRepr.Facets] = deriveCodec[EuLobbyRepr.Facets]
        given Codec[EuLobbyRepr.Results] = deriveCodec[EuLobbyRepr.Results]
        deriveCodec[EuLobbyRepr.RootInterface]

      case "github-events.json" =>
        import GithubEvents.*
        given Codec[Actor] = deriveCodec[Actor]
        given Codec[Author] = deriveCodec[Author]
        given Codec[Self] = deriveCodec[Self]
        given Codec[Owner] = deriveCodec[Owner]
        given Codec[Org] = deriveCodec[Org]
        given Codec[Links] = deriveCodec[Links]
        given Codec[Links1] = deriveCodec[Links1]
        given Codec[Labels] = deriveCodec[Labels]
        given Codec[License] = deriveCodec[License]
        given Codec[Repo] = deriveCodec[Repo]
        given Codec[Repo1] = deriveCodec[Repo1]
        given Codec[Comment] = deriveCodec[Comment]
        given Codec[Comment1] = deriveCodec[Comment1]
        given Codec[Commits] = deriveCodec[Commits]
        given Codec[CreateEvent] = deriveCodec[CreateEvent]
        given Codec[Forkee] = deriveCodec[Forkee]
        given Codec[ForkEvent[Forkee]] = deriveCodec[ForkEvent[Forkee]]
        given Codec[Head[Repo1]] = deriveCodec[Head[Repo1]]
        given Codec[PullRequest] = deriveCodec[PullRequest]
        given Codec[PullRequest1[Head[Repo1]]] = deriveCodec[PullRequest1[Head[Repo1]]]
        given Codec[PullRequest2[Head[Repo1]]] = deriveCodec[PullRequest2[Head[Repo1]]]
        given Codec[PullRequestEvent[Head[Repo1]]] = deriveCodec[PullRequestEvent[Head[Repo1]]]
        given Codec[PullRequestReviewCommentEvent[Head[Repo1]]] = deriveCodec[PullRequestReviewCommentEvent[Head[Repo1]]]
        given Codec[Issue] = deriveCodec[Issue]
        given Codec[Issue1] = deriveCodec[Issue1]
        given Codec[IssueCommentEvent] = deriveCodec[IssueCommentEvent]
        given Codec[IssuesEvent] = deriveCodec[IssuesEvent]
        given Codec[PushEvent] = deriveCodec[PushEvent]
        given Codec[WatchEvent] = deriveCodec[WatchEvent]
        given Codec[RootInterface[Head[Repo1], Forkee]] = deriveCodec[RootInterface[Head[Repo1], Forkee]]
        Codec.of[List[RootInterface[Head[Repo1], Forkee]]]

      case "github-gists.json" =>
        given Codec[GithubGists.Owner] = deriveCodec[GithubGists.Owner]
        given Codec[GithubGists.FileData] = deriveCodec[GithubGists.FileData]
        given Codec[GithubGists.RootInterface] = deriveCodec[GithubGists.RootInterface]
        Codec.of[List[GithubGists.RootInterface]]

      case "json-generator.json" =>
        given Codec[JsonGenerator.Friends] = deriveCodec[JsonGenerator.Friends]
        given Codec[JsonGenerator.Name] = deriveCodec[JsonGenerator.Name]
        given Codec[JsonGenerator.RootInterface] = deriveCodec[JsonGenerator.RootInterface]
        Codec.of[List[JsonGenerator.RootInterface]]

      case "meteorites.json" =>
        given Codec[Meteorites.Geolocation] = deriveCodec[Meteorites.Geolocation]
        given Codec[Meteorites.RootInterface] = deriveCodec[Meteorites.RootInterface]
        Codec.of[List[Meteorites.RootInterface]]

      case "movies.json" =>
        given Codec[Movies.RootInterface] = deriveCodec[Movies.RootInterface]
        Codec.of[List[Movies.RootInterface]]

      case "reddit-scala.json" =>
        given Codec[Reddit.Oembed] = deriveCodec[Reddit.Oembed]
        given Codec[Reddit.SecureMedia] = deriveCodec[Reddit.SecureMedia]
        given Codec[Reddit.MediaEmbed] = deriveCodec[Reddit.MediaEmbed]
        given Codec[Reddit.Gildings] = deriveCodec[Reddit.Gildings]
        given Codec[Reddit.Data] = deriveCodec[Reddit.Data]
        given Codec[Reddit.Child[Reddit.Data]] = deriveCodec[Reddit.Child[Reddit.Data]]
        given Codec[Reddit.Data0[Reddit.Data]] = deriveCodec[Reddit.Data0[Reddit.Data]]
        deriveCodec[Reddit.RootInterface[Reddit.Data]]

      case "rick-morty.json" =>
        given Codec[RickMorty.Rating] = deriveCodec[RickMorty.Rating]
        given Codec[RickMorty.Schedule] = deriveCodec[RickMorty.Schedule]
        given Codec[RickMorty.Country] = deriveCodec[RickMorty.Country]
        given Codec[RickMorty.Network] = deriveCodec[RickMorty.Network]
        given Codec[RickMorty.Image] = deriveCodec[RickMorty.Image]
        given Codec[RickMorty.Externals] = deriveCodec[RickMorty.Externals]
        given Codec[RickMorty.Self] = deriveCodec[RickMorty.Self]
        given Codec[RickMorty.Links] = deriveCodec[RickMorty.Links]
        given Codec[RickMorty.Links1] = deriveCodec[RickMorty.Links1]
        given Codec[RickMorty.Episodes] = deriveCodec[RickMorty.Episodes]
        given Codec[RickMorty.Embedded] = deriveCodec[RickMorty.Embedded]
        deriveCodec[RickMorty.RootInterface]

      case "temp-anomaly.json" =>
        given Codec[TempAnomaly.Description] = deriveCodec[TempAnomaly.Description]
        deriveCodec[TempAnomaly.RootInterface]

      case "thai-cinemas.json" =>
        given Codec[ThaiCinemas.Group] = deriveCodec[ThaiCinemas.Group]
        given Codec[ThaiCinemas.Results] = deriveCodec[ThaiCinemas.Results]
        deriveCodec[ThaiCinemas.RootInterface]

      case "turkish.json" =>
        given Codec[Turkish.Event] = deriveCodec[Turkish.Event]
        given Codec[Turkish.Result] = deriveCodec[Turkish.Result]
        deriveCodec[Turkish.RootInterface]

      case "twitter_api_compact_response.json" | "twitter_api_response.json" =>
        given Codec[TwitterApiResponse.Urls] = deriveCodec[TwitterApiResponse.Urls]
        given Codec[TwitterApiResponse.Url] = deriveCodec[TwitterApiResponse.Url]
        given Codec[TwitterApiResponse.UserMentions] = deriveCodec[TwitterApiResponse.UserMentions]
        given Codec[TwitterApiResponse.Entities] = deriveCodec[TwitterApiResponse.Entities]
        given Codec[TwitterApiResponse.Entities1] = deriveCodec[TwitterApiResponse.Entities1]
        given Codec[TwitterApiResponse.User] = deriveCodec[TwitterApiResponse.User]
        given Codec[TwitterApiResponse.RetweetedStatus] = deriveCodec[TwitterApiResponse.RetweetedStatus]
        given Codec[TwitterApiResponse.RootInterface] = deriveCodec[TwitterApiResponse.RootInterface]
        Codec.of[List[TwitterApiResponse.RootInterface]]

    c.asInstanceOf[Codec[Product]]
  // format: ON

  private given Encoder[Product] = codec.encoder
  private given Decoder[Product] = codec.decoder

  def setup(): Unit = root = Json.decode(fileBytes).to[Product].value

  @Benchmark
  def encodeModel: Array[Byte] = Json.encode(root).withConfig(encodingConfig).toByteArray

  @Benchmark
  def decodeModel: Product = Json.decode(fileBytes).to[Product].value
