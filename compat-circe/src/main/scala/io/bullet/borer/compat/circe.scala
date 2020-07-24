/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import io.bullet.borer._
import io.bullet.borer.encodings.BaseEncoding
import io.circe.{Json, JsonNumber, JsonObject}

import scala.annotation.switch
import scala.collection.compat.Factory

object circe {

  final private val JsonDoubleName = "io.circe.JsonDouble"
  final private val JsonFloatName  = "io.circe.JsonFloat"
  final private val JsonLongName   = "io.circe.JsonLong"

  implicit def borerEncoderFromCirceEncoder[T](implicit ce: io.circe.Encoder[T]): Encoder[T] =
    Encoder((w, x) => circeJsonAstEncoder.write(w, ce(x)))

  implicit def defaultBorerDecoderFromCirceDecoder[T](implicit cd: io.circe.Decoder[T]): Decoder[T] =
    borerDecoderFromCirceDecoder(defaultCirceJsonAstDecoder)

  def borerDecoderFromCirceDecoder[T](circeJsonAstEncoder: Decoder[Json])(
      implicit cd: io.circe.Decoder[T]): Decoder[T] =
    Decoder { r =>
      cd.decodeJson(circeJsonAstEncoder.read(r)) match {
        case Left(e)  => r.validationFailure(e.getMessage())
        case Right(x) => x
      }
    }

  implicit val circeJsonAstEncoder: Encoder[Json] =
    Encoder { (w, value) =>
      value.foldWith {
        new Json.Folder[Writer] {

          def onNull = w.writeNull()

          def onBoolean(value: Boolean) = w.writeBoolean(value)

          def onNumber(value: JsonNumber) =
            if (w.writingCbor) {
              // unfortunately circe's JsonNumber AST is not open for inspection from the outside,
              // so we have to resort to ugly hacks to figure out what _kind_ of JsonNumber it is
              // that we are to serialize
              val className = value.getClass.getName

              if (className eq JsonLongName) w.writeLong(value.toLong.get)
              else if (className eq JsonDoubleName) w.writeDouble(value.toDouble)
              else if (className eq JsonFloatName) w.writeFloat(value.toFloat)
              else
                value.toBigDecimal match {
                  case Some(bigDecimal) =>
                    bigDecimal.toBigIntExact match {
                      case Some(bigInt) => w.write(bigInt)(Encoder.forBigInt)
                      case None         => w.write(bigDecimal)(Encoder.forBigDecimal)
                    }
                  case None =>
                    throw new NumberFormatException(s"The circe JsonNumber `$value` cannot be represented in CBOR")
                }
            } else w.writeNumberString(value.toString)

          def onString(value: String) = w.writeString(value)

          def onArray(value: Vector[Json]) = w.writeIndexedSeq(value)

          def onObject(value: JsonObject) =
            if (value.nonEmpty) {
              val iterator = value.toIterable.iterator
              w.writeMapOpen(value.size)
              while (iterator.hasNext) {
                val (k, v) = iterator.next()
                circeJsonAstEncoder.write(w.writeString(k), v)
              }
              w.writeMapClose()
            } else w.writeEmptyMap()
        }
      }
    }

  implicit val defaultCirceJsonAstDecoder: Decoder[Json] = circeJsonAstDecoder()

  def circeJsonAstDecoder(
      bigIntDecoder: Decoder[BigInt] = Decoder.forBigInt,
      bigDecimalDecoder: Decoder[BigDecimal] = Decoder.forBigDecimal,
      decodeUndefined: Option[() => Json] = Some(() => Json.Null),
      decodeByteArray: Option[Array[Byte] => Json] = Some(defaultDecodeByteArray),
      decodeSimpleValue: Option[SimpleValue => Json] = None): Decoder[Json] = {

    new Decoder[Json] {
      import DataItem.{Shifts => DIS}

      private[this] val arrayDecoder = Decoder.fromFactory(this, implicitly[Factory[Json, Vector[Json]]])
      private[this] val mapDecoder   = Decoder.forListMap(Decoder.forString, this)

      def read(r: Reader) =
        (Integer.numberOfTrailingZeros(r.dataItem()): @switch) match {
          case DIS.Null => r.readNull(); Json.Null

          case DIS.Undefined =>
            decodeUndefined match {
              case Some(f) => f()
              case None    => r.validationFailure("CBOR `undefined` cannot be represented in the circe JSON AST")
            }

          case DIS.Boolean => if (r.readBoolean()) Json.True else Json.False

          case DIS.Int  => Json.fromInt(r.readInt())
          case DIS.Long => Json.fromLong(r.readLong())

          case DIS.OverLong => Json.fromBigInt(bigIntDecoder.read(r))

          case DIS.Float16 | DIS.Float =>
            val float = r.readFloat()
            Json.fromFloat(float) getOrElse r.validationFailure(
              s"Float value `$float` cannot be represented in the circe JSON AST")

          case DIS.Double =>
            val double = r.readDouble()
            Json.fromDouble(double) getOrElse r.validationFailure(
              s"Double value `$double` cannot be represented in the circe JSON AST")

          case DIS.NumberString => Json.fromJsonNumber(JsonNumber.fromDecimalStringUnsafe(r.readNumberString()))

          case DIS.Bytes | DIS.BytesStart =>
            decodeByteArray match {
              case Some(f) => f(r.readByteArray())
              case None    => r.validationFailure("Raw byte arrays cannot be represented in the circe JSON AST")
            }

          case DIS.Chars | DIS.String | DIS.Text | DIS.TextStart => Json.fromString(r.readString())

          case DIS.SimpleValue =>
            decodeSimpleValue match {
              case Some(f) => f(SimpleValue(r.readSimpleValue()))
              case None    => r.validationFailure("Raw byte arrays cannot be represented in the circe JSON AST")
            }

          case DIS.ArrayHeader | DIS.ArrayStart => Json.fromValues(arrayDecoder.read(r))

          case DIS.MapHeader | DIS.MapStart => Json.fromFields(mapDecoder.read(r))

          case DIS.Tag =>
            if (r.hasTag(Tag.PositiveBigNum) | r.hasTag(Tag.NegativeBigNum)) {
              Json.fromBigInt(bigIntDecoder.read(r))
            } else if (r.hasTag(Tag.DecimalFraction)) {
              Json.fromBigDecimal(bigDecimalDecoder.read(r))
            } else r.validationFailure(s"CBOR tag `${r.readTag()}` cannot be represented in the circe JSON AST`")
        }
    }
  }

  private def defaultDecodeByteArray: Array[Byte] => Json =
    bytes => Json.fromString(new String(BaseEncoding.base64.encode(bytes)))
}
