/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import java.nio.ByteBuffer

private[borer] object ByteArrayCache {

  private val threadLocal = new ThreadLocal[Array[Byte]]

  def getBuffer(size: Int): Array[Byte] = {
    var buf = threadLocal.get()
    if ((buf eq null) || buf.length != size) {
      buf = new Array[Byte](size)
      threadLocal.set(buf)
    }
    buf
  }
}

private[borer] object CharArrayCache {

  private val threadLocal = new ThreadLocal[Array[Char]]

  def getBuffer(size: Int): Array[Char] = {
    var buf = threadLocal.get()
    if ((buf eq null) || buf.length != size) {
      buf = new Array[Char](size)
      threadLocal.set(buf)
    }
    buf
  }
}

private[borer] object ByteBufferCache {

  private val threadLocal = new ThreadLocal[ByteBuffer]

  def getBuffer(size: Int): ByteBuffer = {
    var buf = threadLocal.get()
    if ((buf eq null) || buf.capacity != size) {
      buf = ByteBuffer.allocate(size)
      threadLocal.set(buf)
    } else buf.clear()
    buf
  }
}
