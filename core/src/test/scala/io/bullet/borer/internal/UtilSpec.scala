/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import java.nio.charset.StandardCharsets

import io.bullet.borer.Input
import utest._

object UtilSpec extends TestSuite {

  val tests = Tests {

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
