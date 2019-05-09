/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer._
import io.bullet.borer.magnolia._

import scala.annotation.tailrec

object ArrayBasedCodecs {

  object deriveEncoder {
    type Typeclass[T] = Encoder[T]

    def combine[T](ctx: CaseClass[Encoder, T]): Encoder[T] = {
      val params = ctx.parameters
      val len    = params.size
      Encoder { (w, value) =>
        @tailrec def rec(w: Writer, ix: Int): Writer =
          if (ix < len) {
            val p = params(ix)
            rec(p.typeclass.write(w, p.dereference(value)), ix + 1)
          } else w
        len match {
          case 0                  => w.writeEmptyArray()
          case 1                  => rec(w, 0)
          case _ if w.writingJson => rec(w.writeArrayStart(), 0).writeBreak()
          case _                  => rec(w.writeArrayHeader(len), 0)
        }
      }
    }

    def dispatch[T](ctx: SealedTrait[Encoder, T]): Encoder[T] = {
      val subtypes = ctx.subtypesArray
      val len      = subtypes.length
      val typeIds  = key.getTypeIds(ctx.typeName.full, subtypes)
      Encoder { (w, value) =>
        @tailrec def rec(ix: Int): Writer =
          if (ix < len) {
            val sub = subtypes(ix)
            if (sub.cast isDefinedAt value) {
              def writeEntry(w: Writer) = key.Value.write(w, typeIds(ix)).write(sub.cast(value))(sub.typeclass)
              if (w.writingCbor) writeEntry(w.writeArrayHeader(2))
              else writeEntry(w.writeArrayStart()).writeBreak()
            } else rec(ix + 1)
          } else throw new IllegalArgumentException(s"The given value [$value] is not a sub type of [${ctx.typeName}]")
        rec(0)
      }
    }

    def apply[T]: Encoder[T] = macro Magnolia.gen[T]
  }

  object deriveDecoder {
    type Typeclass[T] = Decoder[T]

    def combine[T](ctx: CaseClass[Decoder, T]): Decoder[T] = {
      val params              = ctx.parameters
      val len                 = params.size
      def expected(s: String) = s"$s for decoding an instance of type [${ctx.typeName.full}]"

      Decoder { r =>
        def construct(): T = ctx.construct(_.typeclass.read(r))
        len match {
          case 0 =>
            if (r.tryReadArrayHeader(0) || r.tryReadArrayStart() && r.tryReadBreak()) ctx.construct(null)
            else r.unexpectedDataItem(expected(s"Empty array"))
          case 1 => construct()
          case _ =>
            if (r.tryReadArrayStart()) {
              val result = construct()
              if (r.tryReadBreak()) result
              else r.unexpectedDataItem(expected(s"Array with $len elements"), "at least one extra element")
            } else if (r.tryReadArrayHeader(len)) construct()
            else r.unexpectedDataItem(expected(s"Array Start or Array Header($len)"))
        }
      }
    }

    def dispatch[T](ctx: SealedTrait[Decoder, T]): Decoder[T] = {
      val subtypes            = ctx.subtypesArray
      val typeIds             = key.getTypeIds(ctx.typeName.full, subtypes)
      def expected(s: String) = s"$s for decoding an instance of type [${ctx.typeName.full}]"

      Decoder { r =>
        def readTypeIdAndValue(): T =
          key.tryRead(r, typeIds, 0) match {
            case -1 => r.unexpectedDataItem(s"Any type id key of [${typeIds.map(_.value).mkString(", ")}]")
            case ix => subtypes(ix).typeclass.read(r)
          }

        if (r.tryReadArrayStart()) {
          val result = readTypeIdAndValue()
          if (r.tryReadBreak()) result
          else r.unexpectedDataItem(expected("Array with 2 elements"), "at least one extra element")
        } else if (r.tryReadArrayHeader(2)) {
          readTypeIdAndValue()
        } else r.unexpectedDataItem(expected("Array with 2 elements"))
      }
    }

    def apply[T]: Decoder[T] = macro Magnolia.gen[T]
  }

  def deriveCodec[T]: Codec[T] = macro Macros.deriveCodec[T]
}
