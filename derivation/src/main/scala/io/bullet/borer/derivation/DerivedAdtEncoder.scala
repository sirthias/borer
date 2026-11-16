/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.*

/**
 * Base class of all macro-generated AdtEncoders.
 */
abstract class DerivedAdtEncoder[T] extends AdtEncoder[T] {

  def writeAdtValue[A](w: Writer, typeId: Long, value: A)(using encoder: Encoder[A]): Writer
  def writeAdtValue[A](w: Writer, typeId: String, value: A)(using encoder: Encoder[A]): Writer

  /**
   * Writes a singleton `enum` case (i.e. one without parameters) with the given type ID.
   */
  def writeEnumCase(w: Writer, typeId: Long): Writer
  def writeEnumCase(w: Writer, typeId: String): Writer
}

/**
 * Base class of all macro-generated AdtDecoders.
 */
abstract class DerivedAdtDecoder[T] extends AdtDecoder[T] {

  def failExpectedTypeId(r: Reader): Nothing
}
