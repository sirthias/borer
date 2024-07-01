/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import io.bullet.borer.Float16
import io.bullet.borer.BorerSuite

class UtilSpec extends BorerSuite:

  test("canBeRepresentedAsFloat16") {
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

    def roundTrips(f: Float) =
      val roundTripped = Float16.shortToFloat(Float16.floatToShort(f))
      f == roundTripped || f.isNaN && roundTripped.isNaN

    yes.forall(roundTrips) ==> true
    !no.exists(roundTrips) ==> true
  }
