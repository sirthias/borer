/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import io.bullet.borer.{Reader, Writer}
import io.bullet.borer.magnolia.Subtype

import scala.annotation.{tailrec, StaticAnnotation}

/**
  * Annotation allowing for customizing
  * 1. the id identifying which concrete subtype of an ADT is encoded/to be decoded
  * 2. the name of case class members (de)serialized with [[MapBasedCodecs]]
  *
  * USE CASE 1: Type Ids
  * --------------------
  *
  * By default the derived encoders and decoders for ADTs serialize the short class name of an
  * ADT instance in order to let the decoder now, which subtype to deserialize.
  *
  * This notation allows for customizing the type id that is used to identify an ADT sub-type
  * in the encoding and offers two choices: A [[String]] or an integer number ([[Int]]/[[Long]]) value.
  * (The latter is not supported in JSON and will cause any encoding attempt to fail with an exception!)
  *
  * Relying on strings makes the serialization more descriptive but is more verbose on the wire.
  * Relying on numbers makes the serialization very compact but less descriptive.
  *
  * If necessary styles could also be mixed within a single ADT, even though this is not recommended
  * (for consistency/clarity only, there is no technical reason).
  *
  * The following example shows such an (otherwise not recommended) mixed setup for illustrating the
  * different style of using this annotation for type ids:
  *
  * {{{
  *   sealed trait Animal
  *   object Animal {
  *     @key("D") // default would be "Dog", but we want sth shorter
  *     final case class Dog(name: String, age: Int)
  *
  *     @key(2)
  *     final case class Cat(name: String, age: Int, weight: Option[Double])
  *
  *     // if no @TypeId is given, the short type name is used, in this case "Mouse"
  *     final case class Mouse(name: String, size: Int)
  *   }
  * }}}
  *
  *
  * USE CASE 2: Member Names
  * ------------------------
  *
  * By default [[MapBasedCodecs]] use the case class member name as map key, which is usually a good default.
  * (Note: This will also work with backtick-ed member names as in ``case class(`type`: Int, `foo-bar`: String)``).
  *
  * However, sometimes it's convenient to be able to map the case class member to another name (and vice versa).
  * This annotation allows for just that.
  * As already described above (USE CASE 1) the annotation can take a [[String]] or an integer number ([[Int]]/[[Long]])
  * value as key. Mixing the two different styles in one class is allowed but not recommended.
  * JSON only supports [[String]] keys.
  *
  * This annotation is ignored by [[ArrayBasedCodecs]].
  *
  * Example:
  *
  * {{{
  *   case class Cat(name: String, age: Int, @key("weight") kilos: Option[Double])
  * }}}
  *
  */
final class key(val value: Any) extends StaticAnnotation

object key {

  sealed abstract class Value {
    def value: Any
  }

  object Value {
    final case class Str(value: String) extends Value
    final case class Num(value: Long)   extends Value

    def write(w: Writer, value: Value): w.type =
      value match {
        case Str(x) => w.writeString(x)
        case Num(x) => w.writeLong(x)
      }

    def tryRead(r: Reader, value: Value): Boolean =
      value match {
        case Str(x) => r.tryReadString(x)
        case Num(x) => r.tryReadLong(x)
      }

    def apply(annotation: key): Value =
      annotation.value match {
        case x: String if x.nonEmpty => Str(x)
        case x: Int if x >= 0        => Num(x.toLong)
        case x: Long if x >= 0       => Num(x)
        case _ =>
          sys.error(s"Illegal @key annotation argument: Must be either a non-empty String or a non-negative Int/Long!")
      }
  }

  private[derivation] def find(annotations: Array[Any], default: String, ix: Int = 0): key.Value =
    if (ix < annotations.length) {
      annotations(ix) match {
        case x: key => Value(x)
        case _      => find(annotations, default, ix + 1)
      }
    } else Value.Str(default)

  private[derivation] def getTypeIds[X[_], T](typeName: String, subtypes: Array[Subtype[X, T]]): Array[key.Value] = {
    val typeIds = subtypes.map(sub => key.find(sub.annotationsArray, sub.typeName.short))
    @tailrec def rec(i: Int, j: Int): Array[key.Value] =
      if (i < typeIds.length) {
        if (j < typeIds.length) {
          if (i != j && typeIds(i) == typeIds(j)) {
            sys.error(
              "@key collision: At least two subtypes of [" + typeName +
                s"] share the same type id [${typeIds(i).value}]")
          } else rec(i, j + 1)
        } else rec(i + 1, 0)
      } else typeIds
    rec(0, 0)
  }

  /**
    * Returns the index of the key (in the given `keys` array) that matches the next data item of the given reader,
    * or -1 if none does.
    */
  private[derivation] def tryRead(r: Reader, keys: Array[Value], startIndex: Int): Int = {
    @tailrec def rec(ix: Int, end: Int): Int =
      if (ix < end) {
        val k = keys(ix)
        if (Value.tryRead(r, k)) ix
        else rec(ix + 1, end)
      } else if (startIndex != 0 && end != startIndex) rec(0, startIndex)
      else -1
    rec(startIndex, keys.length)
  }
}
