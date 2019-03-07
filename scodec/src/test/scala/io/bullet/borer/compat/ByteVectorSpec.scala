/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import _root_.scodec.bits.ByteVector
import utest._
import io.bullet.borer.core._
import scodec._

object ByteVectorSpec extends BorerSpec[ByteVector] {

  def newOutput: Output[ByteVector] = newByteVectorOutput
  def newInput(bytes: Array[Byte])  = ByteVector(bytes)

  case class Foo(int: Int, content: ByteVector)

  implicit val fooCodec = Codec(Foo.unapply _, Foo.apply _)

  val tests = Tests {

    "basic roundtrip" - roundTrip(
      "83820B40820C476F682079656168820D43FF0001",
      List(
        Foo(11, ByteVector.empty),
        Foo(12, ByteVector("oh yeah" getBytes "UTF8")),
        Foo(13, ByteVector(-1, 0, 1))
      )
    )
  }
}
