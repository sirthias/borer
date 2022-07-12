/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.site

import io.bullet.borer.BorerSuite

class TranscodingSpec extends BorerSuite {

  test("example") {
    // #example
    import io.bullet.borer.Cbor
    import io.bullet.borer.Dom
    import io.bullet.borer.derivation.MapBasedCodecs._

    case class Employee(nick: String, age: Int)
    case class Department(name: String, employees: List[Employee])

    implicit val employeeCodec   = deriveCodec[Employee]
    implicit val departmentCodec = deriveCodec[Department]

    val value =
      Department(
        name = "IT",
        employees = List(
          Employee("Alice", 32),
          Employee("Bob", 28),
          Employee("Charlie", 42),
        )
      )

    val dom = Cbor // could also be `Json` to target JSON
      .transEncode(value)
      .transDecode
      .to[Dom.Element]
      .value

    val transformer =
      new Dom.Transformer {
        import Dom._

        override def transformMapMember(member: (Element, Element)) =
          member match {
            case (k @ StringElem("age"), IntElem(age)) => k -> IntElem(age + 1)
            case x                                     => super.transformMapMember(x)
          }
      }

    val transformed = transformer(dom)

    val result = Cbor // could also be `Json` to target JSON
      .transEncode(transformed)
      .transDecode
      .to[Department]
      .valueEither

    result ==> Right(
      Department(
        name = "IT",
        employees = List(
          Employee("Alice", 33),
          Employee("Bob", 29),
          Employee("Charlie", 43),
        )
      )
    )
    // #example
  }
}
