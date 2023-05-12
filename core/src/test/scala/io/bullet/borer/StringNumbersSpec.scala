/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

class StringNumbersSpec extends ByteArrayJsonSpec:
  import Decoder.StringNumbers._
  import Decoder.StringBooleans._

  test("Issue 567") {
    verifyDecoding(""""42"""", java.lang.Integer.valueOf(42))
    verifyDecoding(""""true"""", java.lang.Boolean.valueOf(true))
  }

  test("BigInt") {
    verifyDecoding(""""42"""", BigInt(42))
  }

  test("BigDecimal") {
    verifyDecoding(""""42.18"""", BigDecimal("42.18"))
  }
