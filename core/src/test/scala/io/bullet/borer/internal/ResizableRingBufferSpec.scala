/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import utest._

object ResizableRingBufferSpec extends TestSuite {

  def buffers = (0 to 7).map(bit => new ResizableRingBuffer[String](initialCapacity = 1, maxCapacity = 1 << bit))

  val tests = Tests {

    "take in exactly `maxAvailable` elements" - {
      for (buf <- buffers) {
        Iterator.continually("x").takeWhile(buf.append).toArray.length ==> buf.maxCapacity
      }
    }

    "read back exactly the number of elems previously written" - {
      for {
        buf   <- buffers
        count <- Array.tabulate(5)(x => (x + 1) * 31 % buf.maxCapacity)
      } {
        val values = Array.tabulate(count)(_.toString)
        values.foreach(s => buf.append(s) ==> true)
        Array.fill(count)(buf.read()) ==> values
        buf.isEmpty ==> true
        intercept[NoSuchElementException](buf.read())
      }
    }

    "pass a simple stress-test" - {
      for {
        opCount <- Array.tabulate(5)(x => (x + 5) * 31 % 50)
        buf     <- buffers
      } {
        val ops   = Array.tabulate(opCount)(x => (x - 20) * 31 % 50)
        val queue = collection.mutable.Queue[String]()
        val ints  = Iterator.from(0)
        ops foreach {
          case readCount if readCount < 0 =>
            (0 to -readCount).foreach { _ =>
              buf.isEmpty ==> queue.isEmpty
              if (queue.nonEmpty) queue.dequeue() ==> buf.read()
              else intercept[NoSuchElementException](buf.read())
            }
          case writeCount if writeCount > 0 =>
            (0 to writeCount).foreach { _ =>
              val next = ints.next().toString
              if (buf.append(next)) queue.enqueue(next)
            }
          case _ => // ignore
        }
      }
    }
  }
}
