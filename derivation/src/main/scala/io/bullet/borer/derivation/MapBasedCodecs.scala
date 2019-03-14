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

    def dispatch[T](ctx: SealedTrait[Encoder, T]): Encoder[T] =
      ArrayBasedCodecs.deriveEncoder.dispatch(ctx)

    def apply[T]: Encoder[T] = macro Magnolia.gen[T]
  }

  object deriveDecoder {
    type Typeclass[T] = Decoder[T]

    def combine[T](ctx: CaseClass[Decoder, T]): Decoder[T] = {
      val params = ctx.parameters
      val len    = params.size
      Decoder { r ⇒
        val constructorArgs = new Array[AnyRef](len)
        @tailrec def rec(ix: Int): T =
          if (ix < len) {
            val label = r.readString()
            @tailrec def findParam(i: Int): AnyRef =
              if (i < len) {
                val p = params(ix)
                if (p.label == label) p.typeclass.read(r).asInstanceOf[AnyRef]
                else findParam(i + 1)
              } else r.unexpectedDataItem(s"a member of type [${ctx.typeName.full}]", s"member with name [$label]")
            constructorArgs(ix) = findParam(0)
            rec(ix + 1)
          } else ctx.rawConstruct(constructorArgs)

        if (r.readingJson) {
          if (r.tryReadMapStart()) {
            val result = rec(0)
            if (!r.tryReadBreak()) {
              val expected = s"Map with $len elements for decoding an instance of type [${ctx.typeName.full}]"
              r.unexpectedDataItem(expected, "at least one extra element")
            } else result
          } else r.unexpectedDataItem(s"Map Start for decoding an instance of type [${ctx.typeName.full}]")
        } else if (!r.tryReadMapHeader(len)) {
          val expected = s"Map Header with length $len for decoding an instance of type [${ctx.typeName.full}]"
          r.unexpectedDataItem(expected)
        } else rec(0)
      }
    }

    def dispatch[T](ctx: SealedTrait[Decoder, T]): Decoder[T] =
      ArrayBasedCodecs.deriveDecoder.dispatch(ctx)

    def apply[T]: Decoder[T] = macro Magnolia.gen[T]
  }

  def deriveCodec[T]: Codec[T] = macro Macros.deriveCodec[T]
}
