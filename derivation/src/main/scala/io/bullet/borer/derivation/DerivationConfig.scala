/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

/**
 * Allows for custom configuration of certain derivation behaviors.
 *
 * Customizations must be brought into implicit scope for activation, e.g.
 *
 * {{{
 *   implicit val myConfig = DerivationConfig(
 *     encodeCaseClassMemberDefaultValues = true
 *   )
 * }}}
 */
final case class DerivationConfig(
    encodeCaseClassMemberDefaultValues: Boolean = false
)

object DerivationConfig {
  implicit val default: DerivationConfig = DerivationConfig()
}
