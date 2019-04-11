/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal
import io.bullet.borer._

private[borer] abstract class CharArrayOut {

  def writeChar(buf: Array[Char], index: Int, c: Char): Unit

  def write2(buf: Array[Char], index: Int, octa: Long): Unit

  def write3(buf: Array[Char], index: Int, octa: Long): Unit

  def write4(buf: Array[Char], index: Int, octa: Long): Unit

  def write5(buf: Array[Char], index: Int, octa: Long): Unit

  def write6(buf: Array[Char], index: Int, octa: Long): Unit

  def write7(buf: Array[Char], index: Int, octa: Long): Unit

  def write8(buf: Array[Char], index: Int, octa: Long): Unit

  final def write(buf: Array[Char], index: Int, octa: Long, count: Int): Unit =
    count match {
      case 0 ⇒ // nothing to do
      case 1 ⇒ writeChar(buf, index, (octa >>> 56).toChar)
      case 2 ⇒ write2(buf, index, octa)
      case 3 ⇒ write3(buf, index, octa)
      case 4 ⇒ write4(buf, index, octa)
      case 5 ⇒ write5(buf, index, octa)
      case 6 ⇒ write6(buf, index, octa)
      case 7 ⇒ write7(buf, index, octa)
      case 8 ⇒ write8(buf, index, octa)
    }
}

object CharArrayOut {

  final val instance: CharArrayOut = {
    val unsafeCharArrayOut = Unsafe.charArrayOut
    if (unsafeCharArrayOut eq null) {
      new CharArrayOut {
        def writeChar(buf: Array[Char], index: Int, c: Char): Unit =
          buf(index) = c

        def write2(buf: Array[Char], index: Int, octa: Long): Unit = {
          buf(index) = (octa >>> 56).toChar
          buf(index + 1) = ((octa >>> 48) & 0xFFL).toChar
        }

        def write3(buf: Array[Char], index: Int, octa: Long): Unit = {
          buf(index) = (octa >>> 56).toChar
          buf(index + 1) = ((octa >>> 48) & 0xFFL).toChar
          buf(index + 2) = ((octa >>> 40) & 0xFFL).toChar
        }

        def write4(buf: Array[Char], index: Int, octa: Long): Unit = {
          buf(index) = (octa >>> 56).toChar
          buf(index + 1) = ((octa >>> 48) & 0xFFL).toChar
          buf(index + 2) = ((octa >>> 40) & 0xFFL).toChar
          buf(index + 3) = ((octa >>> 32) & 0xFFL).toChar
        }

        def write5(buf: Array[Char], index: Int, octa: Long): Unit = {
          buf(index) = (octa >>> 56).toChar
          buf(index + 1) = ((octa >>> 48) & 0xFFL).toChar
          buf(index + 2) = ((octa >>> 40) & 0xFFL).toChar
          buf(index + 3) = ((octa >>> 32) & 0xFFL).toChar
          buf(index + 4) = ((octa >>> 24) & 0xFFL).toChar
        }

        def write6(buf: Array[Char], index: Int, octa: Long): Unit = {
          buf(index) = (octa >>> 56).toChar
          buf(index + 1) = ((octa >>> 48) & 0xFFL).toChar
          buf(index + 2) = ((octa >>> 40) & 0xFFL).toChar
          buf(index + 3) = ((octa >>> 32) & 0xFFL).toChar
          buf(index + 4) = ((octa >>> 24) & 0xFFL).toChar
          buf(index + 5) = ((octa >>> 16) & 0xFFL).toChar
        }

        def write7(buf: Array[Char], index: Int, octa: Long): Unit = {
          buf(index) = (octa >>> 56).toChar
          buf(index + 1) = ((octa >>> 48) & 0xFFL).toChar
          buf(index + 2) = ((octa >>> 40) & 0xFFL).toChar
          buf(index + 3) = ((octa >>> 32) & 0xFFL).toChar
          buf(index + 4) = ((octa >>> 24) & 0xFFL).toChar
          buf(index + 5) = ((octa >>> 16) & 0xFFL).toChar
          buf(index + 6) = ((octa >>> 8) & 0xFFL).toChar
        }

        def write8(buf: Array[Char], index: Int, octa: Long): Unit = {
          buf(index) = (octa >>> 56).toChar
          buf(index + 1) = ((octa >>> 48) & 0xFFL).toChar
          buf(index + 2) = ((octa >>> 40) & 0xFFL).toChar
          buf(index + 3) = ((octa >>> 32) & 0xFFL).toChar
          buf(index + 4) = ((octa >>> 24) & 0xFFL).toChar
          buf(index + 5) = ((octa >>> 16) & 0xFFL).toChar
          buf(index + 6) = ((octa >>> 8) & 0xFFL).toChar
          buf(index + 7) = (octa & 0xFFL).toChar
        }
      }
    } else unsafeCharArrayOut
  }
}
