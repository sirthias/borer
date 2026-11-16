/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

class ByteStringArrayCodecsSpec extends ByteArrayJsonSpec:

  test("Big Endian - Short Arrays") {
    import ByteStringArrayCodecs.BigEndian.given
    roundTrip(
      """"gADVVgAAKqp//w=="""",
      Array[Short](Short.MinValue, Short.MinValue / 3, 0, Short.MaxValue / 3, Short.MaxValue))
  }

  test("Big Endian - Int Arrays") {
    import ByteStringArrayCodecs.BigEndian.given
    roundTrip(
      """"gAAAANVVVVYAAAAAKqqqqn////8="""",
      Array[Int](Int.MinValue, Int.MinValue / 3, 0, Int.MaxValue / 3, Int.MaxValue))
  }

  test("Big Endian - Long Arrays") {
    import ByteStringArrayCodecs.BigEndian.given
    roundTrip(
      """"gAAAAAAAAADVVVVVVVVVVgAAAAAAAAAAKqqqqqqqqqp//////////w=="""",
      Array[Long](Long.MinValue, Long.MinValue / 3, 0, Long.MaxValue / 3, Long.MaxValue))
  }

  test("Big Endian - Float Arrays") {
    import ByteStringArrayCodecs.BigEndian.given
    roundTrip(
      """"/3////6qqqoAAAAAfqqqqn9///8="""",
      Array[Float](Float.MinValue, Float.MinValue / 3, 0, Float.MaxValue / 3, Float.MaxValue))
  }

  test("Big Endian - Double Arrays") {
    import ByteStringArrayCodecs.BigEndian.given
    roundTrip(
      """"/+//////////1VVVVVVVVQAAAAAAAAAAf9VVVVVVVVV/7////////w=="""",
      Array[Double](Double.MinValue, Double.MinValue / 3, 0, Double.MaxValue / 3, Double.MaxValue))
  }

  test("Little Endian - Short Arrays") {
    import ByteStringArrayCodecs.LittleEndian.given
    roundTrip(
      """"AIBW1QAAqir/fw=="""",
      Array[Short](Short.MinValue, Short.MinValue / 3, 0, Short.MaxValue / 3, Short.MaxValue))
  }

  test("Little Endian - Int Arrays") {
    import ByteStringArrayCodecs.LittleEndian.given
    roundTrip(
      """"AAAAgFZVVdUAAAAAqqqqKv///38="""",
      Array[Int](Int.MinValue, Int.MinValue / 3, 0, Int.MaxValue / 3, Int.MaxValue))
  }

  test("Little Endian - Long Arrays") {
    import ByteStringArrayCodecs.LittleEndian.given
    roundTrip(
      """"AAAAAAAAAIBWVVVVVVVV1QAAAAAAAAAAqqqqqqqqqir/////////fw=="""",
      Array[Long](Long.MinValue, Long.MinValue / 3, 0, Long.MaxValue / 3, Long.MaxValue))
  }

  test("Little Endian - Float Arrays") {
    import ByteStringArrayCodecs.LittleEndian.given
    roundTrip(
      """"//9//6qqqv4AAAAAqqqqfv//f38="""",
      Array[Float](Float.MinValue, Float.MinValue / 3, 0, Float.MaxValue / 3, Float.MaxValue))
  }

  test("Little Endian - Double Arrays") {
    import ByteStringArrayCodecs.LittleEndian.given
    roundTrip(
      """"////////7/9VVVVVVVXV/wAAAAAAAAAAVVVVVVVV1X/////////vfw=="""",
      Array[Double](Double.MinValue, Double.MinValue / 3, 0, Double.MaxValue / 3, Double.MaxValue))
  }
