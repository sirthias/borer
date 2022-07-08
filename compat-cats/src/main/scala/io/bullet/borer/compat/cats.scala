/*
 * Copyright (c) 2019-2022 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import _root_.cats.data._
import _root_.cats.Order
import _root_.cats.instances.order._
import io.bullet.borer._

import scala.collection.immutable.{SortedMap, SortedSet}

object cats {

  implicit def chainEncoder[T: Encoder]: Encoder[Chain[T]] =
    Encoder(_ writeIterator _.iterator)

  implicit def chainDecoder[T: Decoder]: Decoder[Chain[T]] =
    Decoder(r => r.readArrayStart().readUntilBreak(Chain.empty[T])(_ append r.read[T]()))

  implicit def iorEncoder[A: Encoder, B: Encoder]: Encoder[Ior[A, B]] =
    Encoder { (w, x) =>
      x match {
        case Ior.Left(a)    => w.writeArrayOpen(2).writeInt(0).write(a)
        case Ior.Right(b)   => w.writeArrayOpen(2).writeInt(1).write(b)
        case Ior.Both(a, b) => w.writeArrayOpen(3).writeInt(2).write(a).write(b)
      }
      w.writeArrayClose()
    }

  implicit def iorDecoder[A: Decoder, B: Decoder]: Decoder[Ior[A, B]] =
    Decoder { r =>
      var breakExpected = false
      val open = if (r.tryReadArrayStart()) { breakExpected = true; -1L }
      else r.readArrayHeader()
      val result =
        r.readInt() match {
          case 0 if breakExpected || open == 2 => Ior.Left(r.read[A]())
          case 1 if breakExpected || open == 2 => Ior.Right(r.read[B]())
          case 2 if breakExpected || open == 3 => Ior.Both(r.read[A](), r.read[B]())
          case x => r.unexpectedDataItem("Ior encoding", s"ArrayHeader($open) before Int $x")
        }
      if (breakExpected) r.readBreak()
      result
    }

  implicit def necEncoder[T: Encoder]: Encoder[NonEmptyChain[T]] =
    Encoder(_ writeIterator _.iterator)

  implicit def necDecoder[T: Decoder]: Decoder[NonEmptyChain[T]] =
    chainDecoder[T].mapWithReader { (r, chain) =>
      if (chain.nonEmpty) NonEmptyChain.fromChainUnsafe(chain)
      else r.unexpectedDataItem("non-empty chain", "empty chain")
    }

  implicit def nelEncoder[T: Encoder]: Encoder[NonEmptyList[T]] =
    Encoder.forLinearSeq[T, List].contramap(_.toList)

  implicit def nelDecoder[T: Decoder]: Decoder[NonEmptyList[T]] =
    Decoder.fromFactory[T, List].mapWithReader {
      case (_, head :: tail) => NonEmptyList(head, tail)
      case (r, Nil)          => r.unexpectedDataItem("non-empty list", "empty list")
    }

  implicit def nemEncoder[K: Encoder, V: Encoder]: Encoder[NonEmptyMap[K, V]] =
    Encoder.forMap[K, V, SortedMap].contramap(_.toSortedMap)

  implicit def nemDecoder[K: Decoder: Order, V: Decoder]: Decoder[NonEmptyMap[K, V]] =
    Decoder.constructForMap[K, V, SortedMap[K, V]](SortedMap.empty).mapWithReader { (r, sortedMap) =>
      if (sortedMap.nonEmpty) NonEmptyMap.fromMapUnsafe(sortedMap)
      else r.unexpectedDataItem("non-empty map", "empty map")
    }

  implicit def nesEncoder[T: Encoder]: Encoder[NonEmptySet[T]] =
    Encoder.forIterableOnce[T, Set].contramap(_.toSortedSet)

  implicit def nesDecoder[T: Decoder: Order]: Decoder[NonEmptySet[T]] =
    Decoder.fromFactory[T, SortedSet].mapWithReader { (r, set) =>
      if (set.nonEmpty) NonEmptySet.fromSetUnsafe(set)
      else r.unexpectedDataItem("non-empty set", "empty set")
    }

  implicit def nevEncoder[T: Encoder]: Encoder[NonEmptyVector[T]] =
    Encoder.forIndexedSeq[T, Vector].contramap(_.toVector)

  implicit def nevDecoder[T: Decoder]: Decoder[NonEmptyVector[T]] =
    Decoder.fromFactory[T, Vector].mapWithReader { (r, vector) =>
      if (vector.nonEmpty) NonEmptyVector.fromVectorUnsafe(vector)
      else r.unexpectedDataItem("non-empty vector", "empty vector")
    }

  implicit def validatedEncoder[E: Encoder, A: Encoder]: Encoder[Validated[E, A]] =
    Encoder { (w, x) =>
      if (w.writingJson) w.writeArrayStart() else w.writeMapHeader(1)
      x match {
        case Validated.Invalid(e) => w.writeInt(0).write(e)
        case Validated.Valid(a)   => w.writeInt(1).write(a)
      }
      if (w.writingJson) w.writeBreak() else w
    }

  implicit def validatedDecoder[E: Decoder, A: Decoder]: Decoder[Validated[E, A]] =
    Decoder { r =>
      val breakExpected = r.tryReadArrayStart() || { r.readMapHeader(1); false }
      val result =
        r.readInt() match {
          case 0 => Validated.Invalid(r.read[E]())
          case 1 => Validated.Valid(r.read[A]())
          case x => r.unexpectedDataItem(expected = "Int 0 or 1 for decoding a `Validated`", actual = s"Int $x")
        }
      if (breakExpected) r.readBreak()
      result
    }
}
