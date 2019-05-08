/* Magnolia, version 0.10.0. Copyright 2018 Jon Pretty, Propensive Ltd.
 *
 * The primary distribution site is: http://co.ntextu.al/
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 */
package io.bullet.borer.magnolia.examples

import io.bullet.borer.magnolia._

trait SemiDefault[A] {
  def default: A
}

object SemiDefault {
  @inline def apply[A](implicit A: SemiDefault[A]): SemiDefault[A] = A

  type Typeclass[T] = SemiDefault[T]

  def combine[T](ctx: CaseClass[SemiDefault, T]): SemiDefault[T] = new SemiDefault[T] {
    def default = ctx.construct(p => p.default.getOrElse(p.typeclass.default))
  }

  def dispatch[T](ctx: SealedTrait[SemiDefault, T])(): SemiDefault[T] = new SemiDefault[T] {
    def default = ctx.subtypes.head.typeclass.default
  }
  implicit val string: SemiDefault[String] = new SemiDefault[String] { def default = "" }
  implicit val int: SemiDefault[Int]       = new SemiDefault[Int]    { def default = 0  }

  // NOT IMPLICIT
  def gen[T]: SemiDefault[T] = macro Magnolia.gen[T]
}
