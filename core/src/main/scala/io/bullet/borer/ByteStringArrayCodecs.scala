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

  given shortArrayAsByteArrayEncoder(using enc: Encoder[Array[Byte]]): Encoder[Array[Short]] =
    Encoder((w, x) => enc.write(w, ByteArrayAccess.instance.shortArrayToByteArray(x, byteOrder)))

  given intArrayAsByteArrayEncoder(using enc: Encoder[Array[Byte]]): Encoder[Array[Int]] =
    Encoder((w, x) => enc.write(w, ByteArrayAccess.instance.intArrayToByteArray(x, byteOrder)))

  given longArrayAsByteArrayEncoder(using enc: Encoder[Array[Byte]]): Encoder[Array[Long]] =
    Encoder((w, x) => enc.write(w, ByteArrayAccess.instance.longArrayToByteArray(x, byteOrder)))

  given floatArrayAsByteArrayEncoder(using enc: Encoder[Array[Byte]]): Encoder[Array[Float]] =
    Encoder((w, x) => enc.write(w, ByteArrayAccess.instance.floatArrayToByteArray(x, byteOrder)))

  given doubleArrayAsByteArrayEncoder(using enc: Encoder[Array[Byte]]): Encoder[Array[Double]] =
    Encoder((w, x) => enc.write(w, ByteArrayAccess.instance.doubleArrayToByteArray(x, byteOrder)))

  given shortArrayAsByteArrayDecoder(using dec: Decoder[Array[Byte]]): Decoder[Array[Short]] =
    Decoder(r => ByteArrayAccess.instance.byteArrayToShortArray(dec.read(r), byteOrder))

  given intArrayAsByteArrayDecoder(using dec: Decoder[Array[Byte]]): Decoder[Array[Int]] =
    Decoder(r => ByteArrayAccess.instance.byteArrayToIntArray(dec.read(r), byteOrder))

  given longArrayAsByteArrayDecoder(using dec: Decoder[Array[Byte]]): Decoder[Array[Long]] =
    Decoder(r => ByteArrayAccess.instance.byteArrayToLongArray(dec.read(r), byteOrder))

  given floatArrayAsByteArrayDecoder(using dec: Decoder[Array[Byte]]): Decoder[Array[Float]] =
    Decoder(r => ByteArrayAccess.instance.byteArrayToFloatArray(dec.read(r), byteOrder))

  given doubleArrayAsByteArrayDecoder(using dec: Decoder[Array[Byte]]): Decoder[Array[Double]] =
    Decoder(r => ByteArrayAccess.instance.byteArrayToDoubleArray(dec.read(r), byteOrder))

object ByteStringArrayCodecs:

  final val BigEndian    = new ByteStringArrayCodecs(ByteOrder.BIG_ENDIAN)
  final val LittleEndian = new ByteStringArrayCodecs(ByteOrder.LITTLE_ENDIAN)
