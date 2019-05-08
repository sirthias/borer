/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.{Decoder, Encoder}
import io.bullet.borer.magnolia.Subtype

import scala.annotation.{tailrec, StaticAnnotation}

/**
  * Annotation allowing for customizing the type id that automatically derived encoder/decoders
  * for abstract data types produce/consume.
  *
  * By default the derived encoders and decoders for ADTs serialize the short class name of an
  * ADT instance in order to let the decoder now, which sub-type to deserialize.
  *
  * This notation allows for customizing the type id that is used to identify an ADT sub-type
  * in the encoding and offers two choices: A [[String]] or an integer number ([[Int]]/[[Long]]) value.
  *
  * Relying on strings makes the serialization more descriptive but is more verbose on the wire.
  * Relying on numbers makes the serialization very compact but less descriptive.
  *
  * If necessary styles could also be mixed within a single ADT, even though this is not recommended
  * (for consistency/clarity only, there is no technical reason).
  *
  * The following example shows such an (otherwise not recommended) mixed setup for illustrating the
  * different style of using this annotation:
  *
  * {{{
  *   sealed trait Animal
  *   object Animal {
  *     @TypeId("D") // default would be "Dog", but we want sth shorter
  *     final case class Dog(name: String, age: Int)
  *
  *     @TypeId(2)
  *     final case class Cat(name: String, age: Int, weight: Option[Double])
  *
  *     // if no @TypeId is given, the short type name is used, in this case "Mouse"
  *     final case class Mouse(name: String, size: Int)
  *   }
  * }}}
  */
final class TypeId(val value: Any) extends StaticAnnotation

object TypeId {

  sealed trait Value {
    def value: Any
  }

  object Value {
    final case class Str(value: String) extends Value
    final case class Num(value: Long)   extends Value

    implicit val encoder: Encoder[Value] =
      Encoder {
        case (w, Str(x)) ⇒ w.writeString(x)
        case (w, Num(x)) ⇒ w.writeLong(x)
      }

    implicit val decoder: Decoder[Value] =
      Decoder { r ⇒
        if (r.hasString) Str(r.readString())
        else if (r.hasLong) Num(r.readLong())
        else r.unexpectedDataItem("String or Integer for decoding a io.bullet.borer.derivation.TypeId")
      }

    def apply(annotation: TypeId): Value =
      annotation.value match {
        case x: String if x.nonEmpty ⇒ Str(x)
        case x: Int if x >= 0        ⇒ Num(x.toLong)
        case x: Long if x >= 0       ⇒ Num(x)
        case _ ⇒
          sys.error(
            s"Illegal @TypeId annotation argument: Must be either a non-empty String or a non-negative Int/Long!")
      }
  }

  private[derivation] def find(annotations: Array[Any], default: String, ix: Int = 0): TypeId.Value =
    if (ix < annotations.length) {
      annotations(ix) match {
        case x: TypeId ⇒ Value(x)
        case _         ⇒ find(annotations, default, ix + 1)
      }
    } else Value.Str(default)

  private[derivation] def getTypeIds[X[_], T](typeName: String, subtypes: Seq[Subtype[X, T]]): Array[TypeId.Value] = {
    val typeIds = Array.tabulate(subtypes.size) { ix ⇒
      val sub = subtypes(ix)
      TypeId.find(sub.annotationsArray, sub.typeName.short)
    }
    @tailrec def rec(i: Int, j: Int): Array[TypeId.Value] =
      if (i < typeIds.length) {
        if (j < typeIds.length) {
          if (i != j && typeIds(i) == typeIds(j)) {
            sys.error(
              "@TypeId collision: At least two subtypes of [" + typeName +
                s"] share the same TypeId [${typeIds(i).value}]")
          } else rec(i, j + 1)
        } else rec(i + 1, 0)
      } else typeIds
    rec(0, 0)
  }
}
