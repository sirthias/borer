/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.io.Source

class FileSpec extends BorerSuite:

  case class Foo(
      string: String = "This is a really long text for testing writing to a file",
      int: Int = 42,
      double: Double = 0.0)

  given Codec[Foo] = Codec.forProduct[Foo]

  test("small file") {
    val tempFile = File.createTempFile("borer", ".json")
    try
      Json.encode(Foo()).to(tempFile).result ==> tempFile

      new String(Files.readAllBytes(tempFile.toPath), "UTF8") ==>
      """["This is a really long text for testing writing to a file",42,0.0]"""

      Json.decode(tempFile).to[Foo].value ==> Foo()

    finally tempFile.delete()
  }

  test("large file") {
    val testFileBytes = Source.fromResource("large.json").mkString.getBytes(StandardCharsets.UTF_8)
    val config        = Json.DecodingConfig.default
      .copy(maxNumberMantissaDigits = 99, maxNumberAbsExponent = 300, initialCharbufferSize = 8)
    val dom = Json.decode(testFileBytes).withConfig(config).to[Dom.Element].value

    val tempFile = File.createTempFile("borer", ".json")
    try
      Json.encode(dom).to(tempFile).result ==> tempFile

      Json
        .decode(Input.fromFile(tempFile, bufferSize = 256))
        .withConfig(config)
        .to[Dom.Element]
        .value ==> dom

    finally tempFile.delete()
  }
