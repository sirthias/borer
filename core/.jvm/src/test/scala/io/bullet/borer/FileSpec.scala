/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.io.File
import java.nio.file.Files

import utest._

object FileSpec extends TestSuite {

  final case class Foo(
      string: String = "This is a really long text for testing writing to a file",
      int: Int = 42,
      double: Double = 0.0)

  implicit val fooCodec = Codec.forCaseClass[Foo]

  val tests = Tests {

    "read and write files" - {
      val tempFile = File.createTempFile("borer", ".json")
      try {
        Json.encode(Foo()).to(tempFile).bytes ==> tempFile

        new String(Files.readAllBytes(tempFile.toPath), "UTF8") ==>
        """["This is a really long text for testing writing to a file",42,0.0]"""

        Json.decode(tempFile).to[Foo].value ==> Foo()

      } finally tempFile.delete()
    }
  }
}
