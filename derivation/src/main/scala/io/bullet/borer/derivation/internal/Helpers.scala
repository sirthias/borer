/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation.internal

import io.bullet.borer._

object Helpers {

  def failMissing(r: Reader, typeName: String, mask: Int, paramNames: Array[String]): Nothing =
    failMissing(r, typeName, oneBits(~mask & 0xFFFFFFFFL), paramNames)

  def failMissing(r: Reader, typeName: String, mask: Long, paramNames: Array[String]): Nothing =
    failMissing(r, typeName, oneBits(~mask), paramNames)

  def failMissing(r: Reader, typeName: String, m0: Long, m1: Long, paramNames: Array[String]): Nothing =
    failMissing(r, typeName, oneBits(~m0, ~m1), paramNames)

  def failMissing(r: Reader, typeName: String, ones: Iterator[Int], paramNames: Array[String]): Nothing = {
    val missingKeys = ones.map(paramNames(_)).toList
    val misses = missingKeys.lengthCompare(1) match {
      case 0 => s"""key "${missingKeys.head}""""
      case _ => s"""keys "${missingKeys.init.mkString(", ")}" and "${missingKeys.last}""""
    }
    throw new Borer.Error.InvalidInputData(r.position, s"Cannot decode `$typeName` instance due to missing map $misses")
  }

  def readAdtValue[T](r: Reader, typeId: Long)(implicit dec: Decoder[T]) =
    dec match {
      case dec: AdtDecoder[T] => dec.read(r, typeId)
      case _                  => dec.read(r)
    }

  def readAdtValue[T](r: Reader, typeId: String)(implicit dec: Decoder[T]) =
    dec match {
      case dec: AdtDecoder[T] => dec.read(r, typeId)
      case _                  => dec.read(r)
    }

  private def oneBits(m0: Long, m1: Long): Iterator[Int] = oneBits(m0) ++ oneBits(m1).map(_ + 64)
  private def oneBits(mask: Long): Iterator[Int]         = Iterator.from(0).take(64).filter(i => ((mask >>> i) & 1) != 0)
}
