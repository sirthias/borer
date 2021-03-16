/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import scala.deriving.Mirror

type XIterableOnce[+T] = IterableOnce[T]

type XIterableOnceBound[+T] = IterableOnce[T]

def unapplyOption[T <: Product](f: T => T)(using m: Mirror.ProductOf[T]): T => Option[m.MirroredElemTypes] =
  x => Some(Tuple.fromProduct(x).asInstanceOf[m.MirroredElemTypes])
