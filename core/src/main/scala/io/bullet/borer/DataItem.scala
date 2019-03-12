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

  final val Int      = 1 << 3
  final val Long     = 1 << 4
  final val OverLong = 1 << 5
  final val Float16  = 1 << 6
  final val Float    = 1 << 7
  final val Double   = 1 << 8

  final val BigInteger = 1 << 9
  final val BigDecimal = 1 << 10

  final val String    = 1 << 11
  final val Text      = 1 << 12
  final val TextStart = 1 << 13

  final val Bytes      = 1 << 14
  final val BytesStart = 1 << 15

  final val ArrayHeader = 1 << 16
  final val ArrayStart  = 1 << 17

  final val MapHeader = 1 << 18
  final val MapStart  = 1 << 19

  final val Break = 1 << 20
  final val Tag   = 1 << 21

  final val SimpleValue = 1 << 22

  final val EndOfInput = 1 << 23

  //////////////////////////////// COMPOUND ////////////////////////////////

  final val None        = 0
  final val AllButBreak = 0x00FFFFFF & ~Break

  //////////////////////////////////////////////////////////////////////////

  def stringify(mask: Int): String =
    if (mask != AllButBreak) {
      Iterator
        .range(0, 24)
        .map { i ⇒
          mask & (1 << i) match {
            case None ⇒ ""

            case Null      ⇒ "Null"
            case Undefined ⇒ "Undefined"
            case Bool      ⇒ "Bool"

            case Int      ⇒ "Int"
            case Long     ⇒ "Long"
            case OverLong ⇒ "OverLong"
            case Float16  ⇒ "Float16"
            case Float    ⇒ "Float"
            case Double   ⇒ "Double"

            case BigInteger ⇒ "BigInteger"
            case BigDecimal ⇒ "BigDecimal"

            case String    ⇒ "String"
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
    } else "Any Data-Item except for BREAK"
}
