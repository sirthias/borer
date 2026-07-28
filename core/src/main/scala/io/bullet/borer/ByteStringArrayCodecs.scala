/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import io.bullet.borer.internal.{ByteArrayAccess, DirectByteArrayAccess}

import java.nio.ByteOrder

final class ByteStringArrayCodecs private (byteOrder: ByteOrder):

  given shortArrayAsByteArrayEncoder(using enc: Encoder[Array[Byte]]): Encoder[Array[Short]] =
    Encoder((w, x) => enc.write(w, DirectByteArrayAccess.shortArrayToByteArray(x, byteOrder)))

  given intArrayAsByteArrayEncoder(using enc: Encoder[Array[Byte]]): Encoder[Array[Int]] =
    Encoder((w, x) => enc.write(w, DirectByteArrayAccess.intArrayToByteArray(x, byteOrder)))

  given longArrayAsByteArrayEncoder(using enc: Encoder[Array[Byte]]): Encoder[Array[Long]] =
    Encoder((w, x) => enc.write(w, DirectByteArrayAccess.longArrayToByteArray(x, byteOrder)))

  given floatArrayAsByteArrayEncoder(using enc: Encoder[Array[Byte]]): Encoder[Array[Float]] =
    Encoder((w, x) => enc.write(w, DirectByteArrayAccess.floatArrayToByteArray(x, byteOrder)))

  given doubleArrayAsByteArrayEncoder(using enc: Encoder[Array[Byte]]): Encoder[Array[Double]] =
    Encoder((w, x) => enc.write(w, DirectByteArrayAccess.doubleArrayToByteArray(x, byteOrder)))

  given shortArrayAsByteArrayDecoder(using dec: Decoder[Array[Byte]]): Decoder[Array[Short]] =
    Decoder(r => DirectByteArrayAccess.byteArrayToShortArray(dec.read(r), byteOrder))

  given intArrayAsByteArrayDecoder(using dec: Decoder[Array[Byte]]): Decoder[Array[Int]] =
    Decoder(r => DirectByteArrayAccess.byteArrayToIntArray(dec.read(r), byteOrder))

  given longArrayAsByteArrayDecoder(using dec: Decoder[Array[Byte]]): Decoder[Array[Long]] =
    Decoder(r => DirectByteArrayAccess.byteArrayToLongArray(dec.read(r), byteOrder))

  given floatArrayAsByteArrayDecoder(using dec: Decoder[Array[Byte]]): Decoder[Array[Float]] =
    Decoder(r => DirectByteArrayAccess.byteArrayToFloatArray(dec.read(r), byteOrder))

  given doubleArrayAsByteArrayDecoder(using dec: Decoder[Array[Byte]]): Decoder[Array[Double]] =
    Decoder(r => DirectByteArrayAccess.byteArrayToDoubleArray(dec.read(r), byteOrder))

object ByteStringArrayCodecs:

  final val BigEndian    = new ByteStringArrayCodecs(ByteOrder.BIG_ENDIAN)
  final val LittleEndian = new ByteStringArrayCodecs(ByteOrder.LITTLE_ENDIAN)
