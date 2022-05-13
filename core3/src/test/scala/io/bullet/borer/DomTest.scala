/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import utest._

import scala.collection.immutable.HashMap

object DomTest extends TestSuite:

  val tests = Tests {

    "Dom.MapElem.toMap" - {
      Dom.MapElem
        .Sized(
          "age"  -> Dom.IntElem(2),
          "name" -> Dom.StringElem("Lolle")
        )
        .to[HashMap] ==> Map(
        Dom.StringElem("age")  -> Dom.IntElem(2),
        Dom.StringElem("name") -> Dom.StringElem("Lolle")
      )
    }

    "Dom.MapElem.toStringKeyedMap" - {
      Dom.MapElem
        .Sized(
          "age"  -> Dom.IntElem(2),
          "name" -> Dom.StringElem("Lolle")
        )
        .toStringKeyed[HashMap] ==> Right(
        Map(
          "age"  -> Dom.IntElem(2),
          "name" -> Dom.StringElem("Lolle")
        ))
    }
  }
