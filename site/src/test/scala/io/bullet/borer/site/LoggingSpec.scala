/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.site

import io.bullet.borer.BorerSuite

class LoggingSpec extends BorerSuite {

  test("Logging") {
    def `only compiled, not actually run`() = {
      // #example
      import io.bullet.borer.Cbor
      import io.bullet.borer.Codec.ForEither.default

      val value = Map(
        "foo" -> Left(42),
        "bar" -> Right(Vector("For", "the", "King!"))
      )
      val encoded = Cbor.encode(value).toByteArray
      val decoded =
        Cbor
          .decode(encoded)
          .withPrintLogging() // just insert this line to enable logging
          .to[Map[String, Either[Int, Vector[String]]]]
          .value
      // #example
    }
  }
}
