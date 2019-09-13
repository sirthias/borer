/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.nio.charset.StandardCharsets

import io.bullet.borer
import io.bullet.borer.internal.{ElementDeque, Util}

import scala.annotation.tailrec

sealed abstract class AdtEncodingStrategy {
  def writeAdtEnvelopeOpen(w: Writer, typeName: String): Unit
  def writeAdtEnvelopeClose(w: Writer, typeName: String): w.type

  def readAdtEnvelopeOpen(r: Reader, typeName: String): Boolean
  def readAdtEnvelopeClose(r: Reader, openResult: Boolean, typeName: String): Unit
}

object AdtEncodingStrategy {

  /**
    * Default and recommended ADT encoding strategy.
    * Encodes the ADT super type as a single-element map with the only member consisting of the type ID as key
    * and the instance encoding as the value.
    *
    * Example:
    *
    * A `Dog` instance from this ADT:
    *
    * {{{
    *   sealed trait Animal
    *   case class Dog(age: Int, name: String)                      extends Animal
    *   case class Cat(weight: Double, color: String, home: String) extends Animal
    *   case class Mouse(tail: Boolean)                             extends Animal
    * }}}
    *
    * would be encoded as such, for example:
    *
    * {{{
    *   {
    *     "Dog" :  {
    *       "age": 2,
    *       "name": "Lolle"
    *      }
    *   }
    * }}}
    */
  implicit object Default extends AdtEncodingStrategy {

    def writeAdtEnvelopeOpen(w: Writer, typeName: String): Unit =
      if (w.writingJson) w.writeMapStart()
      else w.writeMapHeader(1)

    def writeAdtEnvelopeClose(w: Writer, typeName: String): w.type =
      if (w.writingJson) w.writeBreak() else w

    def readAdtEnvelopeOpen(r: Reader, typeName: String) = {
      def fail() = r.unexpectedDataItem(s"Single-entry Map for decoding an instance of type `$typeName`")
      if (r.tryReadMapStart()) {
        true
      } else if (r.tryReadMapHeader(1)) {
        false
      } else fail()
    }

    def readAdtEnvelopeClose(r: Reader, openResult: Boolean, typeName: String): Unit = {
      def fail() =
        r.unexpectedDataItem(
          s"Single-entry Map for decoding an instance of type `$typeName`",
          "at least one extra element")
      if (openResult && !r.tryReadBreak()) fail()
    }
  }

  /**
    * Alternative ADT encoding strategy, which writes the type ID as an extra map member.
    * The extra member will be the first member in the encoding.
    * (But can be anywhere during decoding. The earlier the type member appears in the encoding map the better
    * the decoding performance and the lesser the caching memory requirements will be.)
    *
    * Requires that all ADT sub types encode to a map.
    * Less efficient (with regard to encoding/decoding process as well as encoded size) than the default strategy.
    *
    * Example:
    *
    * A `Dog` instance from this ADT:
    *
    * {{{
    *   sealed trait Animal
    *   case class Dog(age: Int, name: String)                      extends Animal
    *   case class Cat(weight: Double, color: String, home: String) extends Animal
    *   case class Mouse(tail: Boolean)                             extends Animal
    * }}}
    *
    * would be encoded as such, for example:
    *
    * {{{
    *   {
    *     "_type": "Dog",
    *     "age": 2,
    *     "name": "Lolle"
    *   }
    * }}}
    */
  def flat(typeMemberName: String = "_type", maxBufferSize: Int = 1024 * 1024): AdtEncodingStrategy =
    new AdtEncodingStrategy {
      if (!Util.isPowerOf2(maxBufferSize))
        throw new IllegalArgumentException(s"maxBufferSize must be a power of 2 but was $maxBufferSize")

      private lazy val typeMemberNameBytes = typeMemberName.getBytes(StandardCharsets.UTF_8)

      def writeAdtEnvelopeOpen(w: Writer, typeName: String): Unit = {
        val originalReceiver = w.receiver
        w.receiver = new borer.Receiver.WithDefault {
          private var longTypeId: Long     = _
          private var stringTypeId: String = _
          private var state: Int           = _ // 0 = initial, 1 = int type ID, 2 = long type ID, 3 = string type ID

          override def onInt(value: Int): Unit =
            if (state == 0) {
              longTypeId = value.toLong
              state = 1
            } else default(s"the Int `$value`")

          override def onLong(value: Long): Unit =
            if (state == 0) {
              longTypeId = value
              state = 2
            } else default(s"the Long `$value`")

          override def onString(value: String): Unit =
            if (state == 0) {
              stringTypeId = value
              state = 3
            } else default("a String")

          override def onMapHeader(length: Long): Unit = {
            originalReceiver.onMapHeader(length + 1)
            writeTypeIdMemberAndResetReceiver()
          }

          override def onMapStart(): Unit = {
            originalReceiver.onMapStart()
            writeTypeIdMemberAndResetReceiver()
          }

          private def writeTypeIdMemberAndResetReceiver(): Unit = {
            originalReceiver.onString(typeMemberName)
            state match {
              case 0 => throw new IllegalStateException // a `Map` type ID ?
              case 1 => originalReceiver.onInt(longTypeId.toInt)
              case 2 => originalReceiver.onLong(longTypeId)
              case 3 => originalReceiver.onString(stringTypeId)
            }
            w.receiver = originalReceiver
          }
          protected def default(t: String): Unit =
            throw new Borer.Error.Unsupported(
              w.output,
              s"AdtEncodingStrategy.flat requires all sub-types of `$typeName` be serialized as a Map but here it was $t")
        }
      }

      def writeAdtEnvelopeClose(w: Writer, typeName: String): w.type = w

      def readAdtEnvelopeOpen(r: Reader, typeName: String): Boolean = {
        def failNoMap() =
          r.unexpectedDataItem(s"Map for decoding an instance of type `$typeName`")
        def failNoTypeId() =
          r.unexpectedDataItem(s"Type-ID member `$typeMemberName` for decoding an instance of type `$typeName`")

        def lastTextDataItemMatchesTypeMemberName(stash: ElementDeque) = {
          val byteAccess = stash.dataItemValueFromEnd(-1).asInstanceOf[ByteAccess[AnyRef]]
          val bytes      = stash.dataItemValueFromEnd(-2)
          java.util.Arrays.equals(byteAccess.toByteArray(bytes), typeMemberNameBytes)
        }

        var _prevStash = r.stash
        val stash =
          if ((_prevStash ne null) && _prevStash.isEmpty) {
            _prevStash = null
            r.stash                                          // the existing stash is empty and can be reused
          } else new ElementDeque(maxBufferSize, _prevStash) // we need a new stash level for ourselves
        r.stash = stash
        val prevStash = _prevStash // "finalize" the var

        @tailrec def rec(remaining: Long, mapSize: Long): Unit =
          if (remaining != 0) {
            def finish(): Unit = {
              if (mapSize < 0) stash.prependReceiver.onMapStart() // inject artificial map-start data item
              else stash.prependReceiver.onMapHeader(mapSize - 1) // inject artificial map-header data item
              r.receiveInto(stash.prependReceiver, prevStash)     // member value, i.e. type id
            }

            stash.appendElementFrom(r, prevStash) match {
              case DataItem.String | DataItem.Chars if stash.dataItemValueFromEnd(-1) == typeMemberName =>
                stash.dropLastStringDataItem()
                finish()

              case DataItem.Text if lastTextDataItemMatchesTypeMemberName(stash) =>
                stash.dropLastTextDataItem()
                finish()

              case DataItem.Break | DataItem.EndOfInput => failNoTypeId()

              case _ =>
                stash.appendElementFrom(r, prevStash) // member value
                rec(remaining - 1, mapSize)
            }
          } else failNoTypeId()

        r.receiveInto(stash.appendReceiver, prevStash) match {
          case DataItem.MapStart =>
            stash.clear()
            stash.appendReceiver.onMapHeader(1L)
            r.readMapHeader(1L) // consume the MapHeader from the stash (and make it visible to logging / validation)
            rec(-1, -1)
            true

          case DataItem.MapHeader =>
            val mapSize = stash.peekLastLong()
            stash.patchLastLong(1L)
            r.readMapHeader(1L) // consume the MapHeader from the stash (and make it visible to logging / validation)
            rec(mapSize, mapSize)
            false

          case _ => failNoMap()
        }
      }

      def readAdtEnvelopeClose(r: Reader, openResult: Boolean, typeName: String): Unit = ()
    }
}
