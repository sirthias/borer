/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.input

import java.io.File
import java.nio.file.Files

import io.bullet.borer.ByteAccess
import io.bullet.borer.Input.Provider

trait FromFileInput { this: FromByteArrayInput =>

  implicit object FileProvider extends Provider[File] {
    type Bytes = Array[Byte]
    type In    = FromByteArray
    def byteAccess         = ByteAccess.ForByteArray
    def apply(value: File) = new FromByteArray(Files.readAllBytes(value.toPath))
  }

}
