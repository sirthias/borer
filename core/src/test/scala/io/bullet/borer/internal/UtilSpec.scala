/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import java.nio.charset.StandardCharsets

import io.bullet.borer.{Float16, Input}
import utest._

object UtilSpec extends TestSuite {

  val tests = Tests {

    "canBeRepresentedAsFloat16" - {
      val floats = Seq(
        Float.NaN,
        Float.NegativeInfinity,
        Float.PositiveInfinity,
        Float.MinPositiveValue,
        Float.MinValue,
        Float.MaxValue,
        0.0f,
        -0.0f,
        1.0f,
        -1.0f,
        1.6f,
        -1.6f,
        10.0f,
        -10.0f,
        100.0f,
        -100.0f,
        1000.0f,
        -1000.0f,
        10000.0f,
        -10000.0f,
        100000.0f,
        -100000.0f,
        1000000.0f,
        -1000000.0f
      )

      val (yes, no) = floats.partition(Util.canBeRepresentedAsFloat16)

      def roundTrips(f: Float) = {
        val roundTripped = Float16.shortToFloat(Float16.floatToShort(f))
        f == roundTripped || f.isNaN && roundTripped.isNaN
      }

      yes.forall(roundTrips) ==> true
      !no.exists(roundTrips) ==> true
    }

    "stringCompare" - {

      val fragments = Seq("", "foo", "árvíztűrő ütvefúrógép!", "เอส เอฟ ซีเนม่า เอ็มบีเค เซ็นเตอร์", "飞机因此受到损伤")
      val strings   = for { a <- fragments; b <- fragments; c <- fragments } yield s"$a$b$c"

      for {
        a <- strings
        b <- strings
      } {
        val input         = new Input.FromByteArray(a getBytes StandardCharsets.UTF_8)
        val testCompare   = math.signum(Util.stringCompare(input, b))
        val stringCompare = math.signum(a compareTo b)

        if (testCompare != stringCompare) {
          throw new java.lang.AssertionError(s"""testCompare: $testCompare for "$a" <?> "$b"""")
        }
      }
    }
  }
}
