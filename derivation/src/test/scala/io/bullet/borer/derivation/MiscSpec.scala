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

object MiscSpec extends AbstractBorerSpec {

  def encode[T: Encoder](value: T): String   = Json.encode(value).toUtf8String
  def decode[T: Decoder](encoded: String): T = Json.decode(encoded getBytes StandardCharsets.UTF_8).to[T].value

  final case class CaseClass3(abc: Int, d: String, efghi: Boolean)

  final case class CaseClassT[T](key: String, value: T)

  final case class CaseClass1(flag: Boolean)

  object CaseClass1 {
    def apply(): CaseClass1 = new CaseClass1(false)
  }

  final case class CaseClass1T[T](value: T)

  val tests = Tests {

    "Case Class with 3 members" - {
      implicit val codec = ArrayBasedCodecs.deriveCodec[CaseClass3]
      roundTrip("""[42,"",true]""", CaseClass3(42, "", efghi = true))
    }

    "Generic Case Class with fixed codec" - {
      implicit val codec = ArrayBasedCodecs.deriveCodec[CaseClassT[Double]]
      roundTrip("""["foo",18.1]""", CaseClassT("foo", 18.1))
    }

    "Generic Case Class with generic codec" - {
      implicit def codec[T: Encoder: Decoder]: Codec[CaseClassT[T]] = ArrayBasedCodecs.deriveCodec[CaseClassT[T]]
      roundTrip("""["foo",18.1]""", CaseClassT("foo", 18.1))
    }

    "Unary Case Class with custom apply" - {
      implicit val codec = ArrayBasedCodecs.deriveCodec[CaseClass1]
      roundTrip("false", CaseClass1(false))
    }

    "Generic unary Case Class" - {
      implicit def codec[T: Encoder: Decoder]: Codec[CaseClass1T[T]] = ArrayBasedCodecs.deriveCodec[CaseClass1T[T]]
      roundTrip(""""foo"""", CaseClass1T("foo"))
    }

    "Local Case Class" - {
      case class Box(id: String)
      implicit val boxCodec = ArrayBasedCodecs.deriveCodec[Box]
      roundTrip(""""abc"""", Box("abc"))
    }

    "Recursive Case Class" - {
      case class Box(x: Option[Box] = None)
      implicit lazy val codec: Codec[Box] = ArrayBasedCodecs.deriveCodec[Box]
      roundTrip("""[[[]]]""", Box(Some(Box(Some(Box())))))
    }

    "Deep Derivation" - {
      sealed trait Animal
      case object Wolverine             extends Animal
      case class Yeti(name: String)     extends Animal
      sealed trait Dog                  extends Animal
      case object TheHound              extends Dog
      case class Labrador(name: String) extends Dog
      sealed trait Cat                  extends Animal
      case object TheLastTiger          extends Cat
      case class Lion(color: String)    extends Cat

      "simple" - {
        implicit val codec = ArrayBasedCodecs.deriveAllCodecs[Animal]
        roundTrip("""["Labrador","Lolle"]""", Labrador("Lolle"): Animal)
        roundTrip("""["TheLastTiger",[]]""", TheLastTiger: Animal)
      }

      "custom leaf" - {
        implicit val lionCodec = Codec.bimap[Int, Lion](_ => 0, _ => Lion("roar"))
        implicit val codec     = ArrayBasedCodecs.deriveAllCodecs[Animal]
        roundTrip("""["Lion",0]""", Lion("roar"): Animal)
      }

      "custom inner node" - {
        implicit val catCodec = Codec.bimap[Int, Cat](_ => 0, _ => TheLastTiger)
        implicit val codec    = ArrayBasedCodecs.deriveAllCodecs[Animal]

        roundTrip("""["Cat",0]""", TheLastTiger: Animal)
        verifyEncoding(Lion("roar"): Animal, """["Cat",0]""")
        verifyDecoding("""["Cat",0]""", TheLastTiger: Animal)
      }
    }

    "Deep Derivation on Recursive ADTs" - {
      sealed trait TreeNode
      case object Leaf                                 extends TreeNode
      case class Node(left: TreeNode, right: TreeNode) extends TreeNode

      implicit lazy val codec: Codec[TreeNode] = ArrayBasedCodecs.deriveAllCodecs[TreeNode]

      roundTrip(
        """["Node",[["Node",[["Leaf",[]],["Node",[["Leaf",[]],["Leaf",[]]]]]],["Leaf",[]]]]""",
        Node(Node(Leaf, Node(Leaf, Leaf)), Leaf): TreeNode)
    }

    "Deep Derivation on generic ADTs" - {
      sealed trait Node[+T]
      case object Empty                                   extends Node[Nothing]
      case class Leaf[T](value: T)                        extends Node[T]
      case class Branch[T](left: Node[T], right: Node[T]) extends Node[T]

      implicit def codec[T: Encoder: Decoder]: Codec[Node[T]] = ArrayBasedCodecs.deriveAllCodecs[Node[T]]

      roundTrip(
        """["Branch",[["Branch",[["Leaf",1],["Branch",[["Empty",[]],["Leaf",2]]]]],["Leaf",3]]]""",
        Branch(Branch(Leaf(1), Branch(Empty, Leaf(2))), Leaf(3)): Node[Int])
    }

    "Deep Derivation on ADTs with circular dependencies" - {
      sealed trait ExprX
      case class Add(left: Factor, right: Factor)  extends ExprX
      sealed trait Factor                          extends ExprX
      case class Mult(left: Factor, right: Factor) extends Factor
      case class Literal(value: Int)               extends Factor
      case class Parens(expr: ExprX)               extends Factor

      implicit lazy val factorCodec: Codec[Factor] = ArrayBasedCodecs.deriveAllCodecs[Factor]
      implicit lazy val exprCodec: Codec[ExprX]    = ArrayBasedCodecs.deriveAllCodecs[ExprX]

      roundTrip(
        """["Add",[["Literal",18],["Parens",["Add",[["Literal",2],["Mult",[["Literal",3],["Literal",4]]]]]]]]""",
        Add(Literal(18), Parens(Add(Literal(2), Mult(Literal(3), Literal(4))))): ExprX)
    }

    "Deep Derivation on ADTs with cross dependencies" - {

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

    "CompactMapBasedCodecs" - {

      "unary" - {
        implicit val codec = CompactMapBasedCodecs.deriveCodec[CaseClass1]
        roundTrip("false", CaseClass1(false))
      }

      "non-unary" - {
        implicit val codec = CompactMapBasedCodecs.deriveCodec[CaseClass3]
        roundTrip("""{"abc":42,"d":"","efghi":true}""", CaseClass3(42, "", efghi = true))
      }
    }
  }
}
