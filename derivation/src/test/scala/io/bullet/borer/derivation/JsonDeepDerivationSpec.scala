/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation

import java.nio.charset.StandardCharsets

import io.bullet.borer._
import utest._

object JsonDeepDerivationSpec extends AbstractBorerSpec {

  def encode[T: Encoder](value: T): String   = Json.encode(value).toUtf8String
  def decode[T: Decoder](encoded: String): T = Json.decode(encoded getBytes StandardCharsets.UTF_8).to[T].value

  sealed trait Animal

  object Animal {
    sealed abstract class Dog extends Animal
    sealed trait Cat          extends Animal

    case object Wolverine             extends Animal
    case class Yeti(name: String)     extends Animal
    case object TheHound              extends Dog
    case class Labrador(name: String) extends Dog
    case object TheLastTiger          extends Cat
    case class Lion(color: String)    extends Cat
  }

  val tests = Tests {

    "simple" - {
      implicit val codec = ArrayBasedCodecs.deriveAllCodecs[Animal]
      roundTrip("""["Labrador","Lolle"]""", Animal.Labrador("Lolle"): Animal)
      roundTrip("""["TheLastTiger",[]]""", Animal.TheLastTiger: Animal)
    }

    "custom leaf" - {
      implicit val lionCodec = Codec.bimap[Int, Animal.Lion](_ => 0, _ => Animal.Lion("roar"))
      implicit val codec     = ArrayBasedCodecs.deriveAllCodecs[Animal]
      roundTrip("""["Lion",0]""", Animal.Lion("roar"): Animal)
    }

    "custom inner node" - {
      implicit val catCodec = Codec.bimap[Int, Animal.Cat](_ => 0, _ => Animal.TheLastTiger)
      implicit val codec    = ArrayBasedCodecs.deriveAllCodecs[Animal]

      roundTrip("""["Cat",0]""", Animal.TheLastTiger: Animal)
      verifyEncoding(Animal.Lion("roar"): Animal, """["Cat",0]""")
      verifyDecoding("""["Cat",0]""", Animal.TheLastTiger: Animal)
    }

    "recursive" - {
      sealed trait TreeNode
      case object Leaf                                 extends TreeNode
      case class Node(left: TreeNode, right: TreeNode) extends TreeNode

      implicit lazy val codec: Codec[TreeNode] = ArrayBasedCodecs.deriveAllCodecs[TreeNode]

      roundTrip(
        """["Node",[["Node",[["Leaf",[]],["Node",[["Leaf",[]],["Leaf",[]]]]]],["Leaf",[]]]]""",
        Node(Node(Leaf, Node(Leaf, Leaf)), Leaf): TreeNode)
    }

    "generic" - {
      sealed trait Node[+T]
      case object Empty                                   extends Node[Nothing]
      case class Leaf[T](value: T)                        extends Node[T]
      case class Branch[T](left: Node[T], right: Node[T]) extends Node[T]

      implicit def codec[T: Encoder: Decoder]: Codec[Node[T]] = ArrayBasedCodecs.deriveAllCodecs[Node[T]]

      roundTrip(
        """["Branch",[["Branch",[["Leaf",1],["Branch",[["Empty",[]],["Leaf",2]]]]],["Leaf",3]]]""",
        Branch(Branch(Leaf(1), Branch(Empty, Leaf(2))), Leaf(3)): Node[Int])
    }

    "circular" - {
      sealed trait Expr
      case class Add(left: Factor, right: Factor)  extends Expr
      sealed trait Factor                          extends Expr
      case class Mult(left: Factor, right: Factor) extends Factor
      case class Literal(value: Int)               extends Factor
      case class Parens(expr: Expr)                extends Factor

      implicit lazy val factorCodec: Codec[Factor] = ArrayBasedCodecs.deriveAllCodecs[Factor]
      implicit lazy val exprCodec: Codec[Expr]     = ArrayBasedCodecs.deriveAllCodecs[Expr]

      roundTrip(
        """["Add",[["Literal",18],["Parens",["Add",[["Literal",2],["Mult",[["Literal",3],["Literal",4]]]]]]]]""",
        Add(Literal(18), Parens(Add(Literal(2), Mult(Literal(3), Literal(4))))): Expr)
    }

    "cross dependencies" - {

      "A" - {
        sealed trait Expr
        case class Literal(value: Int) extends Expr
        case class Neg(lit: Literal)   extends Expr

        implicit lazy val exprCodec: Codec[Expr] = ArrayBasedCodecs.deriveAllCodecs[Expr]
      }

      "B" - {
        sealed trait Expr
        case class Neg(lit: Literal)   extends Expr
        case class Literal(value: Int) extends Expr

        implicit lazy val exprCodec: Codec[Expr] = ArrayBasedCodecs.deriveAllCodecs[Expr]
      }
    }

    "diamond" - {
      sealed trait A
      sealed trait B       extends A
      sealed trait C       extends A
      case class D(x: Int) extends B with C
      implicit lazy val codec: Codec[A] = MapBasedCodecs.deriveAllCodecs

      roundTrip("""{"D":{"x":42}}""", D(42): A)
    }

    "stacked" - {
      sealed trait A
      sealed trait B             extends A
      case class C(x: Option[B]) extends B

      implicit lazy val bCodec: Codec[B] = MapBasedCodecs.deriveAllCodecs[B]
      implicit val aCodec                = MapBasedCodecs.deriveAllCodecs[A]

      roundTrip("""{"C":{"x":[]}}""", C(None): A)
    }
  }
}
