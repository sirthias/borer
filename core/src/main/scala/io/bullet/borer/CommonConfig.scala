/*
 * Copyright (c) 2019-2026 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import scala.reflect.Typeable

trait CommonConfig:

  /**
   * The custom context object set via `withContext` on the Encoding API.
   * `null` if not set.
   */
  def rawContext: AnyRef

  /**
   * The custom context object set via `withContext` on the Encoding API.
   * Cast to the given type `A`.
   */
  final def contextAs[A: Typeable]: Option[A] =
    rawContext match
      case x: A => Some(x)
      case _    => None
