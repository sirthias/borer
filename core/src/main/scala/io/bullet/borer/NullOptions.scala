/*
 * Copyright (c) 2019-2024 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

//#docs-quote-delimiter
/**
 * In order to enable an alternative [[Option]] codec, which
 * encodes `None` to `null` and `Some` to an unwrapped value
 * you can import the members of this object with
 *
 * {{{
 * import io.bullet.borer.NullOptions.given
 * }}}
 */
object NullOptions:

  given encoder[T: Encoder]: Encoder[Option[T]] =
    Encoder {
      case (w, Some(x)) => w.write(x)
      case (w, None)    => w.writeNull()
    }

  given decoder[T: Decoder]: Decoder[Option[T]] =
    Decoder { r =>
      if (r.tryReadNull()) None
      else Some(r.read[T]())
    }
//#docs-quote-delimiter
