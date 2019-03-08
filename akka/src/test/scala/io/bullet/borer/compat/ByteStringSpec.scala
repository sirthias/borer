/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import _root_.akka.util.ByteString
import utest._
import io.bullet.borer.core._
import akka._

object ByteStringSpec extends BorerSpec[ByteString] with ByteStringSpecSupport {

  case class Foo(int: Int, content: ByteString)

  implicit val fooCodec = Codec.forCaseClass[Foo]

  val tests = Tests {

    "basic roundtrip" - roundTrip(
      "83820B40820C476F682079656168820D43FF0001",
      List(
        Foo(11, ByteString.empty),
        Foo(12, ByteString("oh yeah")),
        Foo(13, ByteString(-1, 0, 1))
      )
    )
  }
}
