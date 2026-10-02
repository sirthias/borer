/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.immutable.HashMap

class DecodingErrorTest extends BorerSuite:

  given Decoder[Int | Boolean] =
    Decoder.of[Dom.Element].mapEither {
      case x: Dom.IntElem     => Json.transEncode(x).transDecode.to[Int].valueEither
      case x: Dom.BooleanElem => Json.transEncode(x).transDecode.to[Boolean].valueEither
      case x                  => Left("nope")
    }

  test("DecodingError Union Type decomposition") {
    Json.decode("1".getBytes(UTF_8)).to[Int | Boolean].valueEither ==> Right(1)
    Json.decode("true".getBytes(UTF_8)).to[Int | Boolean].valueEither ==> Right(true)
    Json.decode("1.0".getBytes(UTF_8)).to[Int | Boolean].valueEither.swap.map(_.getMessage) ==> Right(
      "nope (input position 0)")
  }
