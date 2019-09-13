/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import _root_.akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import _root_.akka.http.scaladsl.model._
import _root_.akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import _root_.akka.http.scaladsl.util.FastFuture
import _root_.akka.http.scaladsl.util.FastFuture._
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
    * Provides an [[FromEntityUnmarshaller]] for [[T]] given an implicit borer Decoder for [[T]].
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
            case _ =>
              FastFuture.failed(
                Unmarshaller
                  .UnsupportedContentTypeException(MediaTypes.`application/cbor`, MediaTypes.`application/json`)
              )
          }
        } else FastFuture.failed(Unmarshaller.NoContentException)
      }
    }

  /**
    * Provides an [[FromEntityUnmarshaller]] for [[T]] given an implicit borer Decoder for [[T]].
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
            case _               => FastFuture.failed(Unmarshaller.UnsupportedContentTypeException(MediaTypes.`application/cbor`))
          }
        } else FastFuture.failed(Unmarshaller.NoContentException)
      }
    }

  /**
    * Provides an [[FromEntityUnmarshaller]] for [[T]] given an implicit borer Decoder for [[T]].
    * Supports only CBOR with the given MediaType.
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
            case _               => FastFuture.failed(Unmarshaller.UnsupportedContentTypeException(MediaTypes.`application/json`))
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
}
