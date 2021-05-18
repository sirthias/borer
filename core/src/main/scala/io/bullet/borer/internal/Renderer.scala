/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import io.bullet.borer.{Output, Receiver}

/**
 * Common parent type of [[io.bullet.borer.cbor.CborRenderer]] and [[io.bullet.borer.json.JsonRenderer]]
 */
abstract private[borer] class Renderer extends Receiver {
  def out: Output
}
