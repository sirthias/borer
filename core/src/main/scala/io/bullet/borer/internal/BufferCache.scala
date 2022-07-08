/*
 * Copyright (c) 2019-2022 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicReference

private[borer] object ByteArrayCache {

  private val cache = new AtomicReference[Array[Byte]]()

  def acquire(size: Int): Array[Byte] = {
    var buf = cache.getAndSet(null)
    if ((buf eq null) || buf.length != size) buf = new Array[Byte](size)
    buf
  }

  def release(buf: Array[Byte]): Unit = cache.set(buf)
}

private[borer] object CharArrayCache {

  private val cache = new AtomicReference[Array[Char]]()

  def acquire(size: Int): Array[Char] = {
    var buf = cache.getAndSet(null)
    if ((buf eq null) || buf.length != size) buf = new Array[Char](size)
    buf
  }

  def release(buf: Array[Char]): Unit = cache.set(buf)
}

private[borer] object ByteBufferCache {

  private val cache = new AtomicReference[ByteBuffer]()

  def acquire(size: Int): ByteBuffer = {
    var buf = cache.getAndSet(null)
    if ((buf eq null) || buf.capacity != size)
      buf = ByteBuffer.allocate(size)
    else
      buf.clear()
    buf
  }

  def release(buf: ByteBuffer): Unit = cache.set(buf)
}

private[borer] object ElementDequeCache {

  private val cache = new AtomicReference[ElementDeque]()

  def acquire(maxBufferSize: Int): ElementDeque = {
    var deque = cache.getAndSet(null)
    if ((deque eq null) || deque.maxBufferSize != maxBufferSize)
      deque = new ElementDeque(maxBufferSize)
    else
      deque.clear()
    deque
  }

  def release(deque: ElementDeque): Unit = cache.set(deque)
}
