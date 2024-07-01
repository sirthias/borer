/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

/**
 * Abstraction for a "simple value" in CBOR-speak.
 *
 * @param value the value's code
 */
final case class SimpleValue(value: Int):
  if (!SimpleValue.isLegal(value))
    throw new IllegalArgumentException(s"`value` must be in the range ${SimpleValue.legalRange}, but was $value")

object SimpleValue:

  def isLegal(value: Int): Boolean = 0 <= value && value <= 19 || 24 <= value && value <= 255
  def legalRange: String           = "[0..19] or [24..255]"

  given Encoder[SimpleValue] = Encoder((w, x) => w.writeSimpleValue(x.value))
  given Decoder[SimpleValue] = Decoder(r => SimpleValue(r.readSimpleValue()))
