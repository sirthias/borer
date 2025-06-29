/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import io.bullet.borer.*

import scala.compiletime.*
import scala.deriving.*
import scala.Tuple.Size
import scala.annotation.nowarn

private[borer] object BasicProductCodec:

  @nowarn("msg=anonymous class definition will be duplicated at each inline site")
  inline def encoder[T <: Product](using m: Mirror.ProductOf[T]): Encoder[T] =
    type Fields = m.MirroredElemTypes
    inline erasedValue[Fields] match
      case _: EmptyTuple =>
        Encoder[T]((w, _) => w.writeEmptyArray())
      case _: (_ *: EmptyTuple) =>
        Encoder[T]((w, x) => encRec[T, Fields](w, x, 0))
      case _ =>
        Encoder[T]((w, x) => encRec[T, Fields](w.writeArrayOpen(x.productArity), x, 0).writeArrayClose())

  private inline def encRec[T <: Product, Fields <: Tuple](w: Writer, x: T, inline n: Int): Writer =
    inline erasedValue[Fields] match
      case EmptyTuple   => w
      case _: (t *: ts) =>
        encRec[T, ts](w.write(x.productElement(n).asInstanceOf[t])(using summonInline[Encoder[t]]), x, n + 1)

  @nowarn("msg=anonymous class definition will be duplicated at each inline site")
  inline def decoder[T <: Product](using m: Mirror.ProductOf[T]): Decoder[T] =
    type Fields = m.MirroredElemTypes
    inline erasedValue[Fields] match
      case _: EmptyTuple =>
        Decoder[T](r => r.readArrayClose(r.readArrayOpen(0), m.fromProduct(EmptyTuple)))
      case _: (t *: EmptyTuple) =>
        Decoder[T](r => m.fromProduct(Tuple1(r.read[t]()(using summonInline[Decoder[t]]))))
      case _ =>
        val arity = constValue[Size[Fields]]
        Decoder[T](r => r.readArrayClose(r.readArrayOpen(arity), m.fromProduct(decRec[Fields](r))))

  private inline def decRec[T <: Tuple](r: Reader): T =
    inline erasedValue[T] match
      case EmptyTuple   => EmptyTuple.asInstanceOf[T]
      case _: (t *: ts) => (r.read[t]()(using summonInline[Decoder[t]]) *: decRec[ts](r)).asInstanceOf[T]
