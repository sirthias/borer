/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

package object internal {

  type XIterableOnce[+T] = IterableOnce[T]

  type XIterableOnceBound[+T] = IterableOnce[T]

  def unapplyOption[P, T](f: P => Option[T]): P => Option[T] = f
}
