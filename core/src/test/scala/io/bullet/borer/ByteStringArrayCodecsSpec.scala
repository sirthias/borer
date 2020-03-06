/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import utest._

object ByteStringArrayCodecsSpec extends ByteArrayJsonSpec {

  val tests = Tests {

    "Big Endian" - {
      import ByteStringArrayCodecs.BigEndian._

      "Short Arrays" - roundTrip(
        """"gADVVgAAKqp//w=="""",
        Array[Short](Short.MinValue, Short.MinValue / 3, 0, Short.MaxValue / 3, Short.MaxValue))

      "Int Arrays" - roundTrip(
        """"gAAAANVVVVYAAAAAKqqqqn////8="""",
        Array[Int](Int.MinValue, Int.MinValue / 3, 0, Int.MaxValue / 3, Int.MaxValue))

      "Long Arrays" - roundTrip(
        """"gAAAAAAAAADVVVVVVVVVVgAAAAAAAAAAKqqqqqqqqqp//////////w=="""",
        Array[Long](Long.MinValue, Long.MinValue / 3, 0, Long.MaxValue / 3, Long.MaxValue))

      "Float Arrays" - roundTrip(
        """"/3////6qqqoAAAAAfqqqqn9///8="""",
        Array[Float](Float.MinValue, Float.MinValue / 3, 0, Float.MaxValue / 3, Float.MaxValue))

      "Double Arrays" - roundTrip(
        """"/+//////////1VVVVVVVVQAAAAAAAAAAf9VVVVVVVVV/7////////w=="""",
        Array[Double](Double.MinValue, Double.MinValue / 3, 0, Double.MaxValue / 3, Double.MaxValue))

    }

    "Little Endian" - {
      import ByteStringArrayCodecs.LittleEndian._

      "Short Arrays" - roundTrip(
        """"AIBW1QAAqir/fw=="""",
        Array[Short](Short.MinValue, Short.MinValue / 3, 0, Short.MaxValue / 3, Short.MaxValue))

      "Int Arrays" - roundTrip(
        """"AAAAgFZVVdUAAAAAqqqqKv///38="""",
        Array[Int](Int.MinValue, Int.MinValue / 3, 0, Int.MaxValue / 3, Int.MaxValue))

      "Long Arrays" - roundTrip(
        """"AAAAAAAAAIBWVVVVVVVV1QAAAAAAAAAAqqqqqqqqqir/////////fw=="""",
        Array[Long](Long.MinValue, Long.MinValue / 3, 0, Long.MaxValue / 3, Long.MaxValue))

      "Float Arrays" - roundTrip(
        """"//9//6qqqv4AAAAAqqqqfv//f38="""",
        Array[Float](Float.MinValue, Float.MinValue / 3, 0, Float.MaxValue / 3, Float.MaxValue))

      "Double Arrays" - roundTrip(
        """"////////7/9VVVVVVVXV/wAAAAAAAAAAVVVVVVVV1X/////////vfw=="""",
        Array[Double](Double.MinValue, Double.MinValue / 3, 0, Double.MaxValue / 3, Double.MaxValue))

    }

  }
}
