/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import java.nio.charset.StandardCharsets

import utest._

object ReceptacleSpec extends TestSuite {

  val tests = Tests {

    "stringCompareBytes" - {

      val fragments = Seq("", "foo", "árvíztűrő ütvefúrógép!", "เอส เอฟ ซีเนม่า เอ็มบีเค เซ็นเตอร์", "飞机因此受到损伤")
      val strings   = for { a <- fragments; b <- fragments; c <- fragments } yield s"$a$b$c"

      val receptacle = new Receptacle

      for {
        a <- strings
        b <- strings
      } {
        receptacle.onText(a getBytes StandardCharsets.UTF_8)

        val receptacleCompare = math.signum(receptacle.stringCompareBytes(b))
        val stringCompare     = math.signum(a compareTo b)

        if (receptacleCompare != stringCompare) {
          throw new java.lang.AssertionError(s"""receptacleCompare: $receptacleCompare for "$a" <?> "$b"""")
        }
      }
    }
  }
}
