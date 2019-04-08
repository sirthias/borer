/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

/**
  * A simple mapping of CBOR data item types to bits for efficient masking.
  */
object DataItem {

  //////////////////////////////// BASIC ////////////////////////////////

  final val Null      = 1 << 0
  final val Undefined = 1 << 1
  final val Bool      = 1 << 2

  final val Int          = 1 << 3
  final val Long         = 1 << 4
  final val OverLong     = 1 << 5
  final val Float16      = 1 << 6
  final val Float        = 1 << 7
  final val Double       = 1 << 8
  final val Decimal      = 1 << 9
  final val NumberString = 1 << 10

  final val String    = 1 << 11
  final val Chars     = 1 << 12
  final val Text      = 1 << 13
  final val TextStart = 1 << 14

  final val Bytes      = 1 << 15
  final val BytesStart = 1 << 16

  final val ArrayHeader = 1 << 17
  final val ArrayStart  = 1 << 18

  final val MapHeader = 1 << 19
  final val MapStart  = 1 << 20

  final val Break = 1 << 21
  final val Tag   = 1 << 22

  final val SimpleValue = 1 << 23

  final val EndOfInput = 1 << 24

  //////////////////////////////// COMPOUND ////////////////////////////////

  final val None        = 0
  final val StringLike  = String | Chars
  final val AllButBreak = ((EndOfInput << 1) - 1) & ~Break

  //////////////////////////////////////////////////////////////////////////

  def stringify(mask: Int): String =
    if (mask != AllButBreak) {
      Iterator
        .range(0, 25)
        .map { i ⇒
          mask & (1 << i) match {
            case None ⇒ ""

            case Null      ⇒ "Null"
            case Undefined ⇒ "Undefined"
            case Bool      ⇒ "Bool"

            case Int          ⇒ "Int"
            case Long         ⇒ "Long"
            case OverLong     ⇒ "OverLong"
            case Float16      ⇒ "Float16"
            case Float        ⇒ "Float"
            case Double       ⇒ "Double"
            case Decimal      ⇒ "Decimal"
            case NumberString ⇒ "NumberString"

            case String    ⇒ "String"
            case Chars     ⇒ "Chars"
            case Text      ⇒ "Text"
            case TextStart ⇒ "Start of unbounded Text"

            case Bytes      ⇒ "Bytes"
            case BytesStart ⇒ "Start of unbounded Bytes"

            case ArrayHeader ⇒ "Array"
            case ArrayStart  ⇒ "Start of unbounded Array"

            case MapHeader ⇒ "Map"
            case MapStart  ⇒ "Start of unbounded Map"

            case Break ⇒ "BREAK"
            case Tag   ⇒ "Tag"

            case SimpleValue ⇒ "Simple Value"

            case EndOfInput ⇒ "End of Input"
          }
        }
        .filter(_.nonEmpty)
        .take(java.lang.Integer.bitCount(mask))
        .toList match {
        case Nil      ⇒ "none"
        case x :: Nil ⇒ x
        case x        ⇒ x.init.mkString("", ", ", " or " + x.last)
      }
    } else "Any DataItem except BREAK"
}
