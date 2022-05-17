/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import munit.FunSuite
import scala.annotation.{implicitNotFound, targetName}

abstract class BorerSuite extends FunSuite:

  extension [A](lhs: A)
    @targetName("arrowEquals")
    def ==>[B](rhs: B)(using CanEqual[A, B], munit.Location): Unit =
      def arrayToSeq(x: Any) =
        x match
          case a: Array[_] => a.toSeq
          case _           => x

      assertEquals(arrayToSeq(lhs), arrayToSeq(rhs))
