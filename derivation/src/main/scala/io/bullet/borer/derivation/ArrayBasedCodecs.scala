/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.{Codec, Decoder, Encoder, Writer}

import scala.annotation.tailrec
import scala.collection.mutable
import magnolia._

object ArrayBasedCodecs {

  object deriveEncoder {
    type Typeclass[T] = Encoder[T]

    def combine[T](ctx: CaseClass[Encoder, T]): Encoder[T] = {
      val params = ctx.parameters
      val len    = params.size
      Encoder { (w, value) ⇒
        @tailrec def rec(w: Writer, ix: Int): w.type =
          if (ix < len) {
            val p = params(ix)
            rec(p.typeclass.write(w, p.dereference(value)), ix + 1)
          } else w
        len match {
          case 0                  ⇒ w.writeEmptyArray()
          case 1                  ⇒ rec(w, 0)
          case _ if w.writingJson ⇒ rec(w.writeArrayStart(), 0).writeBreak()
          case _                  ⇒ rec(w.writeArrayHeader(len), 0)
        }
      }
    }

    def dispatch[T](ctx: SealedTrait[Encoder, T]): Encoder[T] = {
      val subtypes = ctx.subtypes
      val len      = subtypes.size
      val typeIds  = getTypeIds(ctx.typeName.full, subtypes)
      Encoder { (w, value) ⇒
        @tailrec def rec(ix: Int): Unit =
          if (ix < len) {
            val sub = subtypes(ix)
            if (sub.cast isDefinedAt value) {
              w.writeToArray(typeIds(ix), sub.cast(value))(TypeId.Value.encoder, sub.typeclass)
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
      val params = ctx.parameters
      val len    = params.size
      Decoder { r ⇒
        @tailrec def rec(ix: Int, constructorArgs: Array[AnyRef] = new Array(len)): T =
          if (ix < len) {
            constructorArgs(ix) = params(ix).typeclass.read(r).asInstanceOf[AnyRef]
            rec(ix + 1, constructorArgs)
          } else ctx.rawConstruct(constructorArgs)

        len match {
          case 0 ⇒ ctx.rawConstruct(Nil)
          case 1 ⇒ rec(0)
          case _ ⇒
            if (r.readingJson) {
              if (r.tryReadArrayStart()) {
                val result = rec(0)
                if (!r.tryReadBreak()) {
                  val expected = s"Array with $len elements for decoding an instance of type [${ctx.typeName.full}]"
                  r.unexpectedDataItem(expected, "at least one extra element")
                } else result
              } else r.unexpectedDataItem(s"Array Start for decoding an instance of type [${ctx.typeName.full}]")
            } else if (!r.tryReadArrayHeader(len)) {
              val expected = s"Array Header with length $len for decoding an instance of type [${ctx.typeName.full}]"
              r.unexpectedDataItem(expected)
            } else rec(0)
        }
      }
    }

    def dispatch[T](ctx: SealedTrait[Decoder, T]): Decoder[T] = {
      val subtypes = ctx.subtypes.asInstanceOf[mutable.WrappedArray[Subtype[Decoder, T]]].array
      val typeIds  = getTypeIds(ctx.typeName.full, subtypes)
      Decoder { r ⇒
        @tailrec def rec(id: TypeId.Value, ix: Int): T =
          if (ix < typeIds.length) {
            if (typeIds(ix) == id) subtypes(ix).typeclass.read(r)
            else rec(id, ix + 1)
          } else r.unexpectedDataItem(s"Any TypeId in [${typeIds.map(_.value).mkString(", ")}]", id.value.toString)

        if (r.readingJson && r.tryReadArrayStart()) {
          val result = rec(r.read[TypeId.Value](), 0)
          if (!r.tryReadBreak()) {
            val expected = s"Array with 2 elements for decoding an instance of type [${ctx.typeName.full}]"
            r.unexpectedDataItem(expected, "at least one extra element")
          } else result
        } else if (r.tryReadArrayHeader(2)) {
          rec(r.read[TypeId.Value](), 0)
        } else r.unexpectedDataItem(s"Array with length 2 for decoding an instance of type [${ctx.typeName.full}]")
      }
    }

    def apply[T]: Decoder[T] = macro Magnolia.gen[T]
  }

  def deriveCodec[T]: Codec[T] = macro Macros.deriveCodec[T]

  private def getTypeIds[X[_], T](typeName: String, subtypes: Seq[Subtype[X, T]]): Array[TypeId.Value] = {
    val typeIds = Array.tabulate(subtypes.size) { ix ⇒
      val sub = subtypes(ix)
      TypeId.find(sub.annotationsArray, sub.typeName.short)
    }
    @tailrec def rec(i: Int, j: Int): Array[TypeId.Value] =
      if (i < typeIds.length) {
        if (j < typeIds.length) {
          if (i != j && typeIds(i) == typeIds(j)) {
            sys.error(
              "@TypeId collision: At least two subtypes of [" + typeName +
                s"] share the same TypeId [${typeIds(i).value}]")
          } else rec(i, j + 1)
        } else rec(i + 1, 0)
      } else typeIds
    rec(0, 0)
  }
}
