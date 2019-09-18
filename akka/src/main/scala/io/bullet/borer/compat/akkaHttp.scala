/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import scala.concurrent.Future
import scala.reflect.ClassTag
import _root_.akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import _root_.akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import _root_.akka.http.scaladsl.common.EntityStreamingSupport
import _root_.akka.http.scaladsl.marshalling.{Marshalling, NoStrictlyCompatibleElementMarshallingAvailableException}
import _root_.akka.http.scaladsl.model._
import _root_.akka.http.scaladsl.util.FastFuture
import _root_.akka.http.scaladsl.util.FastFuture._
import _root_.akka.stream.scaladsl.{Flow, Keep, Source}
import _root_.akka.util.ByteString
import _root_.akka.NotUsed
import io.bullet.borer._

/**
  * Automatic to and from CBOR and/or JSON marshalling/unmarshalling using in-scope borer Encoders / Decoders.
  */
object akkaHttp {

  private[this] val byteArrayUnmarshaller: FromEntityUnmarshaller[Array[Byte]] =
    Unmarshaller.byteArrayUnmarshaller

  // brevity aliases
  type CborDecodingSetup = DecodingSetup.Api[Input[Array[Byte]], Cbor.DecodingConfig]
  type JsonDecodingSetup = DecodingSetup.Api[Input[Array[Byte]], Json.DecodingConfig]

  implicit def defaultBorerUnmarshaller[T: Decoder]: FromEntityUnmarshaller[T] = borerUnmarshaller()

  /**
    * Provides a [[FromEntityUnmarshaller]] for [[T]] given an implicit borer Decoder for [[T]].
    * Supports both CBOR and JSON with the given MediaTypes.
    */
  def borerUnmarshaller[T: Decoder](
      cborMediaType: MediaType = MediaTypes.`application/cbor`,
      jsonMediaType: MediaType = MediaTypes.`application/json`,
      configureCbor: CborDecodingSetup => CborDecodingSetup = identity,
      configureJson: JsonDecodingSetup => JsonDecodingSetup = identity
  ): FromEntityUnmarshaller[T] =
    Unmarshaller.withMaterializer { implicit ec => implicit mat => httpEntity =>
      byteArrayUnmarshaller(httpEntity).fast.flatMap { bytes =>
        if (bytes.length > 0) {
          httpEntity.contentType.mediaType match {
            case `cborMediaType` => FastFuture(configureCbor(Cbor.decode(bytes)).to[T].valueTry)
            case `jsonMediaType` => FastFuture(configureJson(Json.decode(bytes)).to[T].valueTry)
            case _               => FastFuture.failed(Unmarshaller.UnsupportedContentTypeException(cborMediaType, jsonMediaType))
          }
        } else FastFuture.failed(Unmarshaller.NoContentException)
      }
    }

  /**
    * Provides a [[FromEntityUnmarshaller]] for [[T]] given an implicit borer Decoder for [[T]].
    * Supports only CBOR with the given MediaType.
    */
  def borerCborUnmarshaller[T: Decoder](
      cborMediaType: MediaType = MediaTypes.`application/cbor`,
      configureCbor: CborDecodingSetup => CborDecodingSetup = identity): FromEntityUnmarshaller[T] =
    Unmarshaller.withMaterializer { implicit ec => implicit mat => httpEntity =>
      byteArrayUnmarshaller(httpEntity).fast.flatMap { bytes =>
        if (bytes.length > 0) {
          httpEntity.contentType.mediaType match {
            case `cborMediaType` => FastFuture(configureCbor(Cbor.decode(bytes)).to[T].valueTry)
            case _               => FastFuture.failed(Unmarshaller.UnsupportedContentTypeException(cborMediaType))
          }
        } else FastFuture.failed(Unmarshaller.NoContentException)
      }
    }

  /**
    * Provides a [[FromEntityUnmarshaller]] for [[T]] given an implicit borer Decoder for [[T]].
    * Supports only JSON with the given MediaType.
    */
  def borerJsonUnmarshaller[T: Decoder](
      jsonMediaType: MediaType = MediaTypes.`application/json`,
      configureJson: JsonDecodingSetup => JsonDecodingSetup = identity
  ): FromEntityUnmarshaller[T] =
    Unmarshaller.withMaterializer { implicit ec => implicit mat => httpEntity =>
      byteArrayUnmarshaller(httpEntity).fast.flatMap { bytes =>
        if (bytes.length > 0) {
          httpEntity.contentType.mediaType match {
            case `jsonMediaType` => FastFuture(configureJson(Json.decode(bytes)).to[T].valueTry)
            case _               => FastFuture.failed(Unmarshaller.UnsupportedContentTypeException(jsonMediaType))
          }
        } else FastFuture.failed(Unmarshaller.NoContentException)
      }
    }

  // brevity aliases
  type CborEncodingSetup = EncodingSetup.Api[Cbor.EncodingConfig]
  type JsonEncodingSetup = EncodingSetup.Api[Json.EncodingConfig]

  implicit def defaultBorerMarshaller[T: Encoder]: ToEntityMarshaller[T] = borerMarshaller()

  /**
    * Provides a [[ToEntityMarshaller]] for [[T]] given an implicit borer Encoder for [[T]].
    * Supports both CBOR and JSON with the given MediaTypes.
    * Content negotiation will determine, whether CBOR or JSON is produced.
    * If the client accepts both the marshaller will produce CBOR.
    */
  def borerMarshaller[T: Encoder](
      cborContentType: ContentType = MediaTypes.`application/cbor`,
      jsonContentType: ContentType = MediaTypes.`application/json`,
      configureCbor: CborEncodingSetup => CborEncodingSetup = identity,
      configureJson: JsonEncodingSetup => JsonEncodingSetup = identity
  ): ToEntityMarshaller[T] = {
    val cborMarshaller = borerCborMarshaller(cborContentType, configureCbor)
    val jsonMarshaller = borerJsonMarshaller(jsonContentType, configureJson)
    Marshaller.oneOf(cborMarshaller, jsonMarshaller)
  }

  /**
    * Provides a [[ToEntityMarshaller]] for [[T]] given an implicit borer Encoder for [[T]].
    * Supports only CBOR with the given MediaType.
    */
  def borerCborMarshaller[T: Encoder](
      cborContentType: ContentType = MediaTypes.`application/cbor`,
      configureCbor: CborEncodingSetup => CborEncodingSetup = identity): ToEntityMarshaller[T] =
    Marshaller.byteArrayMarshaller(cborContentType).compose[T] { value =>
      configureCbor(Cbor.encode(value)).toByteArray
    }

  /**
    * Provides a [[ToEntityMarshaller]] for [[T]] given an implicit borer Encoder for [[T]].
    * Supports only JSON with the given MediaType.
    */
  def borerJsonMarshaller[T: Encoder](
      jsonContentType: ContentType = MediaTypes.`application/json`,
      configureJson: JsonEncodingSetup => JsonEncodingSetup = identity
  ): ToEntityMarshaller[T] =
    Marshaller.byteArrayMarshaller(jsonContentType).compose[T] { value =>
      configureJson(Json.encode(value)).toByteArray
    }

  implicit def defaultBorerJsonStreamUnmarshaller[T: Decoder]: FromEntityUnmarshaller[Source[T, NotUsed]] =
    borerStreamUnmarshaller(EntityStreamingSupport.json())

  /**
    * Provides a [[FromEntityUnmarshaller]] which produces streams of [[T]] given an implicit borer Decoder
    * for [[T]]. Supports JSON or CSV, depending on the given [[EntityStreamingSupport]].
    *
    * @see https://doc.akka.io/api/akka-http/10.1.9/akka/http/scaladsl/common/EntityStreamingSupport.html
    */
  def borerStreamUnmarshaller[T: Decoder](ess: EntityStreamingSupport): FromEntityUnmarshaller[Source[T, NotUsed]] =
    Unmarshaller.withMaterializer { implicit ec => implicit mat =>
      val unmarshalSync = (byteString: ByteString) => Json.decode(byteString.toArray[Byte]).to[T].value
      val unmarshallingFlow =
        if (ess.parallelism > 1) {
          val unmarshalAsync = (byteString: ByteString) => Future(unmarshalSync(byteString))
          if (ess.unordered) Flow[ByteString].mapAsyncUnordered(ess.parallelism)(unmarshalAsync)
          else Flow[ByteString].mapAsync(ess.parallelism)(unmarshalAsync)
        } else Flow[ByteString].map(unmarshalSync)

      httpEntity => {
        if (ess.supported matches httpEntity.contentType) {
          val frames = httpEntity.dataBytes.via(ess.framingDecoder)
          FastFuture.successful(frames.viaMat(unmarshallingFlow)(Keep.right))
        } else FastFuture.failed(Unmarshaller.UnsupportedContentTypeException(ess.supported))
      }
    }

  implicit def defaultBorerJsonStreamMarshaller[T: ToEntityMarshaller: ClassTag]
      : ToEntityMarshaller[Source[T, NotUsed]] =
    borerStreamMarshaller[T](EntityStreamingSupport.json())

  /**
    * Provides a [[ToEntityMarshaller]] for streams of [[T]] given an implicit borer Encoder for [[T]].
    * Supports JSON or CSV, depending on the given [[EntityStreamingSupport]].
    */
  def borerStreamMarshaller[T](ess: EntityStreamingSupport)(
      implicit
      marshaller: ToEntityMarshaller[T],
      classTag: ClassTag[T]): ToEntityMarshaller[Source[T, NotUsed]] = {

    type Marshallings = List[Marshalling[MessageEntity]]
    val contentType = ess.contentType
    val selectMarshalling: Marshallings => Option[() => MessageEntity] =
      contentType match {
        case _: ContentType.Binary | _: ContentType.WithFixedCharset | _: ContentType.WithMissingCharset =>
          (_: Marshallings).collectFirst { case Marshalling.WithFixedContentType(`contentType`, marshal) => marshal }
        case ContentType.WithCharset(mediaType, charset) =>
          (_: Marshallings).collectFirst {
            case Marshalling.WithFixedContentType(`contentType`, marshal) => marshal
            case Marshalling.WithOpenCharset(`mediaType`, marshal)        => () => marshal(charset)
          }
      }

    Marshaller[Source[T, NotUsed], MessageEntity] { implicit ec => source =>
      FastFuture successful {
        Marshalling.WithFixedContentType(contentType, () => {
          val byteStream =
            source
              .mapAsync(1)(value => marshaller(value)(ec))
              .map { marshallings =>
                selectMarshalling(marshallings)
                  .orElse(marshallings collectFirst { case Marshalling.Opaque(x) => x })
                  .getOrElse(
                    throw new NoStrictlyCompatibleElementMarshallingAvailableException[T](contentType, marshallings))
              }
              .flatMapConcat(_.apply().dataBytes) // marshal!
              .via(ess.framingRenderer)

          HttpEntity(contentType, byteStream)
        }) :: Nil
      }
    }
  }
}
