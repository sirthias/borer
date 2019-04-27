/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer._
import io.bullet.borer.Borer.Error
import magnolia._

import scala.annotation.tailrec

object MapBasedCodecs {

  object deriveEncoder {
    type Typeclass[T] = Encoder[T]

    def combine[T](ctx: CaseClass[Encoder, T]): Encoder[T] = {
      val params = ctx.parameters
      val len    = params.size
      Encoder { (w, value) ⇒
        @tailrec def rec(w: Writer, ix: Int): Writer =
          if (ix < len) {
            val p = params(ix)
            w.writeString(p.label)
            rec(p.typeclass.write(w, p.dereference(value)), ix + 1)
          } else w

        if (w.writingJson) rec(w.writeMapStart(), 0).writeBreak()
        else rec(w.writeMapHeader(len), 0)
      }
    }

    @inline def dispatch[T](ctx: SealedTrait[Encoder, T]): Encoder[T] =
      ArrayBasedCodecs.deriveEncoder.dispatch(ctx)

    def apply[T]: Encoder[T] = macro Magnolia.gen[T]
  }

  object deriveDecoder {
    type Typeclass[T] = Decoder[T]

    def combine[T](ctx: CaseClass[Decoder, T]): Decoder[T] = {
      @inline def typeName = ctx.typeName.full
      val params           = ctx.parameters
      val len              = params.size
      if (len > 64) sys.error(s"Cannot derive Decoder[$typeName]: More than 64 members are unsupported")
      def expected(s: String) = s"$s for decoding an instance of type [$typeName]"

      Decoder { r ⇒
        val constructorArgs = new Array[AnyRef](len)

        @tailrec def fillArgsAndConstruct(alreadyFilledCount: Int, filledMask: Long): T = {
          @tailrec def findAndFillNextArg(i: Int, end: Int): Long =
            if (i < end) {
              val p = params(i)
              if (r.tryReadString(p.label)) {
                val mask = 1L << i
                if ((filledMask & mask) == 0) {
                  constructorArgs(i) = p.typeclass.read(r).asInstanceOf[AnyRef]
                  filledMask | mask
                } else throw new Error.InvalidInputData(r.position, s"Duplicate map key [${p.label}] encountered")
              } else findAndFillNextArg(i + 1, end)
            } else if (end == len) findAndFillNextArg(0, alreadyFilledCount)
            else
              r.unexpectedDataItem(
                expected("a map key/member"),
                if (r.hasString) r.read[Dom.Element].toString
                else DataItem.stringify(r.dataItem))

          if (alreadyFilledCount < len) {
            val newFilledMask = findAndFillNextArg(alreadyFilledCount, len)
            fillArgsAndConstruct(alreadyFilledCount + 1, newFilledMask)
          } else ctx.rawConstruct(constructorArgs)
        }

        if (r.tryReadMapStart()) {
          val result = fillArgsAndConstruct(0, 0L)
          if (r.tryReadBreak()) result
          else r.unexpectedDataItem(expected(s"Map with $len elements"), "at least one extra element")
        } else if (r.tryReadMapHeader(len)) fillArgsAndConstruct(0, 0L)
        else r.unexpectedDataItem(expected(s"Map Start or Map Header ($len)"))
      }
    }

    @inline def dispatch[T](ctx: SealedTrait[Decoder, T]): Decoder[T] =
      ArrayBasedCodecs.deriveDecoder.dispatch(ctx)

    def apply[T]: Decoder[T] = macro Magnolia.gen[T]
  }

  def deriveCodec[T]: Codec[T] = macro Macros.deriveCodec[T]
}
