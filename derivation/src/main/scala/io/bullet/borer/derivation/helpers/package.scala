/*
 * Copyright (c) 2019-2022 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation.helpers

import io.bullet.borer.*

def failMissing(r: Reader, typeName: String, mask: Int, fieldNames: List[String]): Nothing =
  failMissing(r, typeName, oneBits(~mask & 0xFFFFFFFFL), fieldNames)

def failMissing(r: Reader, typeName: String, mask: Long, fieldNames: List[String]): Nothing =
  failMissing(r, typeName, oneBits(~mask), fieldNames)

def failMissing(r: Reader, typeName: String, m0: Long, m1: Long, fieldNames: List[String]): Nothing =
  failMissing(r, typeName, oneBits(~m0, ~m1), fieldNames)

def failMissing(r: Reader, typeName: String, ones: Iterator[Int], fieldNames: List[String]): Nothing = {
  val missingKeys = ones.map(fieldNames(_)).toList
  val misses = missingKeys.lengthCompare(1) match {
    case 0 => s"""key "${missingKeys.head}""""
    case _ => s"""keys "${missingKeys.init.mkString(", ")}" and "${missingKeys.last}""""
  }
  throw new Borer.Error.InvalidInputData(r.position, s"Cannot decode `$typeName` instance due to missing map $misses")
}

def failDuplicateMapKey(r: Reader, key: Long | String, typeName: String): Nothing =
  throw new Borer.Error.InvalidInputData(
    r.position,
    s"Duplicate map key `$key` encountered during decoding an instance of type `$typeName`")

def readAdtValue[T, A](r: Reader, typeId: Long, decoder: Decoder[A]): T = {
  val result = decoder match
    case x: AdtDecoder[A] => x.read(r, typeId)
    case x                => x.read(r)
  result.asInstanceOf[T]
}

def readAdtValue[T, A](r: Reader, typeId: String, decoder: Decoder[A]): T =
  val result = decoder match
    case x: AdtDecoder[A] => x.read(r, typeId)
    case x                => x.read(r)
  result.asInstanceOf[T]

private def oneBits(m0: Long, m1: Long): Iterator[Int] = oneBits(m0) ++ oneBits(m1).map(_ + 64)
private def oneBits(mask: Long): Iterator[Int]         = Iterator.from(0).take(64).filter(i => ((mask >>> i) & 1) != 0)
