/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer._
import magnolia._

import scala.annotation.tailrec

object MapBasedCodecs {

  object deriveEncoder {
    type Typeclass[T] = Encoder[T]

    def combine[T](ctx: CaseClass[Encoder, T]): Encoder[T] = {
      val params = ctx.parameters
      val len    = params.size
      Encoder { (w, value) ⇒
        @tailrec def rec(w: Writer, ix: Int): w.type =
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
      val params              = ctx.parameters
      val len                 = params.size
      def typeName            = ctx.typeName.full
      def expected(s: String) = s"$s for decoding an instance of type [$typeName]"

      Decoder { r ⇒
        def construct(): T = ctx.construct { param ⇒
          @tailrec def rec(i: Int, end: Int): AnyRef =
            if (i < end) {
              if (r.tryReadString(param.label)) param.typeclass.read(r).asInstanceOf[AnyRef]
              else rec(i + 1, end)
            } else if (end == len) rec(0, param.index)
            else r.unexpectedDataItem(s"a member of type [$typeName]", s"member with name [${r.readString()}]")
          rec(param.index, len)
        }
        if (r.tryReadMapStart()) {
          val result = construct()
          if (r.tryReadBreak()) result
          else r.unexpectedDataItem(expected(s"Map with $len elements"), "at least one extra element")
        } else if (r.tryReadMapHeader(len)) construct()
        else r.unexpectedDataItem(expected(s"Map Start or Map Header ($len)"))
      }
    }

    @inline def dispatch[T](ctx: SealedTrait[Decoder, T]): Decoder[T] =
      ArrayBasedCodecs.deriveDecoder.dispatch(ctx)

    def apply[T]: Decoder[T] = macro Magnolia.gen[T]
  }

  def deriveCodec[T]: Codec[T] = macro Macros.deriveCodec[T]
}
