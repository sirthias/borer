/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.nio.ByteOrder
import io.bullet.borer.internal.ByteArrayAccess

final class ByteStringArrayCodecs private (byteOrder: ByteOrder):

  implicit def shortArrayAsByteArrayEncoder(implicit enc: Encoder[Array[Byte]]): Encoder[Array[Short]] =
    Encoder((w, x) => enc.write(w, ByteArrayAccess.instance.shortArrayToByteArray(x, byteOrder)))

  implicit def intArrayAsByteArrayEncoder(implicit enc: Encoder[Array[Byte]]): Encoder[Array[Int]] =
    Encoder((w, x) => enc.write(w, ByteArrayAccess.instance.intArrayToByteArray(x, byteOrder)))

  implicit def longArrayAsByteArrayEncoder(implicit enc: Encoder[Array[Byte]]): Encoder[Array[Long]] =
    Encoder((w, x) => enc.write(w, ByteArrayAccess.instance.longArrayToByteArray(x, byteOrder)))

  implicit def floatArrayAsByteArrayEncoder(implicit enc: Encoder[Array[Byte]]): Encoder[Array[Float]] =
    Encoder((w, x) => enc.write(w, ByteArrayAccess.instance.floatArrayToByteArray(x, byteOrder)))

  implicit def doubleArrayAsByteArrayEncoder(implicit enc: Encoder[Array[Byte]]): Encoder[Array[Double]] =
    Encoder((w, x) => enc.write(w, ByteArrayAccess.instance.doubleArrayToByteArray(x, byteOrder)))

  implicit def shortArrayAsByteArrayDecoder(implicit dec: Decoder[Array[Byte]]): Decoder[Array[Short]] =
    Decoder(r => ByteArrayAccess.instance.byteArrayToShortArray(dec.read(r), byteOrder))

  implicit def intArrayAsByteArrayDecoder(implicit dec: Decoder[Array[Byte]]): Decoder[Array[Int]] =
    Decoder(r => ByteArrayAccess.instance.byteArrayToIntArray(dec.read(r), byteOrder))

  implicit def longArrayAsByteArrayDecoder(implicit dec: Decoder[Array[Byte]]): Decoder[Array[Long]] =
    Decoder(r => ByteArrayAccess.instance.byteArrayToLongArray(dec.read(r), byteOrder))

  implicit def floatArrayAsByteArrayDecoder(implicit dec: Decoder[Array[Byte]]): Decoder[Array[Float]] =
    Decoder(r => ByteArrayAccess.instance.byteArrayToFloatArray(dec.read(r), byteOrder))

  implicit def doubleArrayAsByteArrayDecoder(implicit dec: Decoder[Array[Byte]]): Decoder[Array[Double]] =
    Decoder(r => ByteArrayAccess.instance.byteArrayToDoubleArray(dec.read(r), byteOrder))

object ByteStringArrayCodecs:

  final val BigEndian    = new ByteStringArrayCodecs(ByteOrder.BIG_ENDIAN)
  final val LittleEndian = new ByteStringArrayCodecs(ByteOrder.LITTLE_ENDIAN)
