/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.io.BufferedInputStream

import scala.io.Source

class JsonTestSuite extends BorerSuite:

  val disabled: Set[String] = Set(
    "n_multidigit_number_then_00.json",
    "n_structure_null-byte-outside-string.json",
    "n_structure_whitespace_formfeed.json"
  )

  val testFiles =
    Source
      .fromResource("")
      .getLines()
      .map { name =>
        val is = new BufferedInputStream(getClass.getResourceAsStream("/" + name))
        try name -> Iterator.continually(is.read).takeWhile(_ != -1).map(_.toByte).toArray
        finally is.close()
      }
      .toMap
      .view
      .filter(t => !disabled.contains(t._1))

  val config = Json.DecodingConfig.default.copy(maxNumberMantissaDigits = 99, maxNumberAbsExponent = 999)

  test("Accept") {
    for {
      (name, bytes) <- testFiles
      if name startsWith "y"
    }
      Json.decode(bytes).withConfig(config).to[Dom.Element].valueEither match
        case Right(_) => // ok
        case Left(e)  => throw new RuntimeException(s"Test `$name` did not parse as it should", e)
  }

  test("Reject") {
    for {
      (name, bytes) <- testFiles
      if name startsWith "n"
    }
      Json.decode(bytes).withConfig(config).to[Dom.Element].valueEither match
        case Left(_)  => // ok
        case Right(x) => throw new RuntimeException(s"Test `$name` parsed even though it should have failed: $x")
  }

  test("Not Crash") {
    for {
      (name, bytes) <- testFiles
      if name startsWith "i"
    }
      Json.decode(bytes).withConfig(config).to[Dom.Element].valueEither match
        case Right(_)                        => // everything else is fine
        case Left(e: Borer.Error.General[_]) => throw new RuntimeException(s"Test `$name` did fail unexpectedly", e)
        case Left(_)                         => // everything else is fine
  }
