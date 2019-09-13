/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import _root_.akka.actor.ActorSystem
import _root_.akka.http.scaladsl.marshalling.Marshal
import _root_.akka.http.scaladsl.model._
import _root_.akka.http.scaladsl.unmarshalling.Unmarshaller.UnsupportedContentTypeException
import _root_.akka.http.scaladsl.unmarshalling.{Unmarshal, Unmarshaller}
import _root_.akka.stream.ActorMaterializer
import io.bullet.borer.Codec
import utest._
import MediaTypes.{`application/cbor`, `application/json`}

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

/**
  * More or less direct adaptation of
  * https://github.com/hseeberger/akka-http-json/blob/7a700166e962ec77cdea1a222e219ec78ca1175c/akka-http-jsoniter-scala/src/test/scala/de/heikoseeberger/akkahttpjsoniterscala/JsoniterScalaSupportSpec.scala
  * which is copyright by Heiko Seeberger and licensed under Apache License, Version 2.0.
  */
object AkkaHttpSupportSpec extends TestSuite {

  final case class Foo(bar: String) {
    require(bar == "bar", "bar must be 'bar'!")
  }

  implicit private val system: ActorSystem    = ActorSystem()
  implicit private val mat: ActorMaterializer = ActorMaterializer()
  import system.dispatcher

  implicit private val codec: Codec[Foo] = {
    import io.bullet.borer.derivation.MapBasedCodecs._
    deriveCodec[Foo]
  }

  val tests = Tests {

    import akkaHttp._

    "marshalling and unmarshalling (CBOR)" - {
      val foo = Foo("bar")
      Marshal(foo)
        .to[RequestEntity]
        .map { entity =>
          entity.contentType ==> ContentType(`application/cbor`)
          entity
        }
        .flatMap(Unmarshal(_).to[Foo])
        .map(_ ==> foo)
    }

    "marshalling and unmarshalling (JSON)" - {
      val foo          = Foo("bar")
      val acceptHeader = _root_.akka.http.scaladsl.model.headers.Accept(MediaRange.One(`application/json`, 1.0f))
      Marshal(foo)
        .toResponseFor(HttpRequest(headers = acceptHeader :: Nil))
        .flatMap(_.entity.toStrict(1.second))
        .map { entity =>
          entity.contentType ==> ContentTypes.`application/json`
          entity
        }
        .flatMap(Unmarshal(_).to[Foo])
        .map(_ ==> foo)
    }

    "surface error message for requirement errors" - {
      val entity = HttpEntity(`application/json`, """{ "bar": "baz" }""")
      Unmarshal(entity)
        .to[Foo]
        .failed
        .map(_.getMessage ==> "java.lang.IllegalArgumentException: requirement failed: bar must be 'bar'! (input position 15)")
    }

    "fail with NoContentException when unmarshalling empty entities" - {
      val entity = HttpEntity.empty(ContentTypes.`application/json`)
      Unmarshal(entity)
        .to[Foo]
        .failed
        .map(_ ==> Unmarshaller.NoContentException)
    }

    "fail with UnsupportedContentTypeException when Content-Type is not `application/json`" - {
      val entity = HttpEntity("""{ "bar": "bar" }""")
      Unmarshal(entity)
        .to[Foo]
        .failed
        .map(_ ==> UnsupportedContentTypeException(ContentType(`application/cbor`), ContentTypes.`application/json`))
    }
  }

  override def utestAfterAll(): Unit = Await.ready(system.terminate(), 4.seconds)
}
