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
package io.bullet.borer.magnolia

import io.bullet.borer.magnolia.examples._
import utest.{Show ⇒ _, _}

import scala.annotation.StaticAnnotation

sealed trait Tree[+T]
case class Leaf[+L](value: L)                        extends Tree[L]
case class Branch[+B](left: Tree[B], right: Tree[B]) extends Tree[B]

sealed trait Path[+A]
case class Destination[+A](value: A)                    extends Path[A]
case class Crossroad[+A](left: Path[A], right: Path[A]) extends Path[A]
case class OffRoad[+A](path: Option[Path[A]])           extends Path[A]

sealed trait Entity

case class Company(name: String)          extends Entity
case class Person(name: String, age: Int) extends Entity
case class Address(line1: String, occupant: Person)

class Length(val value: Int) extends AnyVal

case class FruitBasket(fruits: Fruit*)
case class Lunchbox(fruit: Fruit, drink: String)
case class Fruit(name: String)
object Fruit {
  implicit val showFruit: Show[String, Fruit] = (f: Fruit) ⇒ f.name
}

case class Item(name: String, quantity: Int = 1, price: Int)

sealed trait Color
case object Red   extends Color
case object Green extends Color
case object Blue  extends Color

case class MyAnnotation(order: Int) extends StaticAnnotation

sealed trait AttributeParent
@MyAnnotation(0) case class Attributed(
    @MyAnnotation(1) p1: String,
    @MyAnnotation(2) p2: Int
) extends AttributeParent

case class `%%`(`/`: Int, `#`: String)

case class Parameter(a: String, b: String)
case class Test(param: Parameter)
object Test {
  def apply(): Test = Test(Parameter("", ""))

  def apply(a: String)(implicit b: Int): Test = Test(Parameter(a, b.toString))

  def apply(a: String, b: String): Test = Test(Parameter(a, b))
}

sealed trait Politician[+S]
case class Accountable[+S](slogan: S)                           extends Politician[S]
case class Corrupt[+S, +L <: Seq[Company]](slogan: S, lobby: L) extends Politician[S]

sealed trait Box[+A]
case class SimpleBox[+A](value: A)                              extends Box[A]
case class LabelledBox[+A, L <: String](value: A, var label: L) extends Box[A]

case class Account(id: String, emails: String*)

case class Portfolio(companies: Company*)

case class Recursive(children: Seq[Recursive])

// This tests compilation.
class GenericCsv[A: Csv]
object ParamCsv extends GenericCsv[Parameter]

class NotDerivable

case class NoDefault(value: Boolean)

final case class ServiceName1(value: String) extends AnyVal
final case class ServiceName2(value: String)

sealed abstract class Halfy
final case class Lefty()  extends Halfy
final case class Righty() extends Halfy

object Tests extends TestSuite {

  val testPackage = "io.bullet.borer.magnolia"

  val tests = Tests {

    "construct a Show product instance with alternative apply functions" - {
      Show.gen[Test].show(Test("a", "b")) ==> """Test(param=Parameter(a=a,b=b))"""
    }

    "construct a Show product instance" - {
      Show.gen[Person].show(Person("John Smith", 34)) ==> """Person(name=John Smith,age=34)"""
    }

    "construct a Show coproduct instance" - {
      Show.gen[Person].show(Person("John Smith", 34)) ==> "Person(name=John Smith,age=34)"
    }

    "serialize a Branch" - {
      implicitly[Show[String, Branch[String]]]
        .show(Branch(Leaf("LHS"), Leaf("RHS"))) ==> "Branch[String](left=Leaf[String](value=LHS),right=Leaf[String](value=RHS))"
    }

    "local implicit beats Magnolia" - {
      implicit val showPerson: Show[String, Person] = new Show[String, Person] {
        def show(p: Person) = "nobody"
      }
      implicitly[Show[String, Address]]
        .show(Address("Home", Person("John Smith", 44))) ==> "Address(line1=Home,occupant=nobody)"
    }

    "even low-priority implicit beats Magnolia for nested case" - {
      implicitly[Show[String, Lunchbox]]
        .show(Lunchbox(Fruit("apple"), "lemonade")) ==> "Lunchbox(fruit=apple,drink=lemonade)"
    }

    "low-priority implicit beats Magnolia when not nested" - {
      implicitly[Show[String, Fruit]].show(Fruit("apple")) ==> "apple"
    }

    "low-priority implicit beats Magnolia when chained" - {
      implicitly[Show[String, FruitBasket]]
        .show(FruitBasket(Fruit("apple"), Fruit("banana"))) ==> "FruitBasket(fruits=[apple,banana])"
    }

    "typeclass implicit scope has lower priority than ADT implicit scope" - {
      implicitly[Show[String, Fruit]].show(Fruit("apple")) ==> "apple"
    }

    "test equality false" - {
      Eq.gen[Entity].equal(Person("John Smith", 34), Person("", 0)) ==> false
    }

    "test equality true" - {
      Eq.gen[Entity].equal(Person("John Smith", 34), Person("John Smith", 34)) ==> true
    }

    "test branch equality true" - {
      Eq.gen[Tree[String]].equal(Branch(Leaf("one"), Leaf("two")), Branch(Leaf("one"), Leaf("two"))) ==> true
    }

    "construct a default value" - {
      HasDefault.gen[Entity].defaultValue ==> Right(Company(""))
    }

//    "construction of Show instance for Leaf" - {
//      scalac"""
//        implicitly[Show[String, Leaf[java.lang.String]]]
//      """ ==> Returns(fqt"magnolia.examples.Show[String,magnolia.tests.Leaf[String]]")
//    }
//
//    "construction of Show instance for Tree" - {
//      scalac"""
//        implicitly[Show[String, Tree[String]]]
//      """ ==> Returns(fqt"magnolia.examples.Show[String,magnolia.tests.Tree[String]]")
//    }

    "serialize a Leaf" - {
      implicitly[Show[String, Leaf[String]]].show(Leaf("testing")) ==> "Leaf[String](value=testing)"
    }

    "serialize a Branch as a Tree" - {
      implicitly[Show[String, Tree[String]]]
        .show(Branch(Leaf("LHS"), Leaf("RHS"))) ==> "Branch[String](left=Leaf[String](value=LHS),right=Leaf[String](value=RHS))"
    }

    "serialize case object" - {
      implicitly[Show[String, Red.type]].show(Red) ==> "Red()"
    }

    "access default constructor values" - {
      implicitly[HasDefault[Item]].defaultValue ==> Right(Item("", 1, 0))
    }

    "serialize case object as a sealed trait" - {
      implicitly[Show[String, Color]].show(Blue) ==> "Blue()"
    }

    "decode a company" - {
      Decoder.gen[Company].decode("""Company(name=Acme Inc)""") ==> Company("Acme Inc")
    }

    "decode a Person as an Entity" - {
      implicitly[Decoder[Entity]]
        .decode(s"""$testPackage.Person(name=John Smith,age=32)""") ==> Person("John Smith", 32)
    }

    "decode a nested product" - {
      implicitly[Decoder[Address]].decode(
        """Address(line1=53 High Street,occupant=Person(name=Richard Jones,age=44))"""
      ) ==> Address("53 High Street", Person("Richard Jones", 44))
    }

//    "show error stack" - {
//      scalac"""
//        case class Alpha(integer: Double)
//        case class Beta(alpha: Alpha)
//        Show.gen[Beta]
//      """ ==> TypecheckError(txt"""magnolia: could not find Show.Typeclass for type Double
//      |    in parameter 'integer' of product type Alpha
//      |    in parameter 'alpha' of product type Beta
//      |""")
//    }

//    "not attempt to instantiate Unit when producing error stack" - {
//      scalac"""
//        case class Gamma(unit: Unit)
//        Show.gen[Gamma]
//      """ ==> TypecheckError(txt"""magnolia: could not find Show.Typeclass for type Unit
//      |    in parameter 'unit' of product type Gamma
//      |""")
//    }

//    "not assume full auto derivation of external value classes" - {
//      scalac"""
//        case class LoggingConfig(n: ServiceName1)
//        object LoggingConfig {
//          implicit val semi: SemiDefault[LoggingConfig] = SemiDefault.gen
//        }
//        """ ==> TypecheckError(txt"""magnolia: could not find SemiDefault.Typeclass for type magnolia.tests.ServiceName1
//    in parameter 'n' of product type LoggingConfig
//""")
//    }

//    "not assume full auto derivation of external products" - {
//      scalac"""
//        case class LoggingConfig(n: ServiceName2)
//        object LoggingConfig {
//          implicit val semi: SemiDefault[LoggingConfig] = SemiDefault.gen
//        }
//        """ ==> TypecheckError(txt"""magnolia: could not find SemiDefault.Typeclass for type magnolia.tests.ServiceName2
//    in parameter 'n' of product type LoggingConfig
//""")
//    }

//    "not assume full auto derivation of external coproducts" - {
//      scalac"""
//        case class LoggingConfig(o: Option[String])
//        object LoggingConfig {
//          implicit val semi: SemiDefault[LoggingConfig] = SemiDefault.gen
//        }
//        """ ==> TypecheckError(txt"""magnolia: could not find SemiDefault.Typeclass for type Option[String]
//    in parameter 'o' of product type LoggingConfig
//""")
//    }

    "half auto derivation of sealed families" - {
      SemiDefault.gen[Halfy].default ==> Lefty()
    }

    "typenames and labels are not encoded" - {
      implicitly[Show[String, `%%`]].show(`%%`(1, "two")) ==> "%%(/=1,#=two)"
    }

    "serialize a tuple" - {
      val tupleDerivation = implicitly[Show[String, (Int, String)]]
      tupleDerivation.show((42, "Hello World")) ==> "Tuple2[Int,String](_1=42,_2=Hello World)"
    }

    "serialize a value class" - {
      Show.gen[Length].show(new Length(100)) ==> "100"
    }

    // Corrupt being covariant in L <: Seq[Company] enables the derivation for Corrupt[String, _]
    "show a Politician with covariant lobby" - {
      Show
        .gen[Politician[String]]
        .show(Corrupt("wall", Seq(Company("Alice Inc")))) ==> "Corrupt[String,Seq[Company]](slogan=wall,lobby=[Company(name=Alice Inc)])"
    }

    // LabelledBox being invariant in L <: String prohibits the derivation for LabelledBox[Int, _]
//    "can't show a Box with invariant label" - {
//      scalac"Show.gen[Box[Int]]" ==>
//      TypecheckError(txt"""magnolia: could not find Show.Typeclass for type L
//      |    in parameter 'label' of product type magnolia.tests.LabelledBox[Int, _ <: String]
//      |    in coproduct type magnolia.tests.Box[Int]
//      |""")
//    }

    "patch a Person via a Patcher[Entity]" - {
      // these two implicits can be removed once https://github.com/propensive/magnolia/issues/58 is closed
      implicit val stringPatcher = Patcher.forSingleValue[String]
      implicit val intPatcher    = Patcher.forSingleValue[Int]

      val person = Person("Bob", 42)
      implicitly[Patcher[Entity]].patch(person, Seq(null, 21)) ==> Person("Bob", 21)
    }

    "throw on an illegal patch attempt with field count mismatch" - {
      // these two implicits can be removed once https://github.com/propensive/magnolia/issues/58 is closed
      implicit val stringPatcher = Patcher.forSingleValue[String]
      implicit val intPatcher    = Patcher.forSingleValue[Int]

      val person = Person("Bob", 42)
      val e      = intercept[RuntimeException](implicitly[Patcher[Entity]].patch(person, Seq(null, 21, 'killer)))
      e.getMessage ==> "Cannot patch value `Person(Bob,42)`, expected 2 fields but got 3"
    }

    "throw on an illegal patch attempt with field type mismatch" - {
      // these two implicits can be removed once https://github.com/propensive/magnolia/issues/58 is closed
      implicit val stringPatcher = Patcher.forSingleValue[String]
      implicit val intPatcher    = Patcher.forSingleValue[Int]

      val person = Person("Bob", 42)
      val msg    = intercept[RuntimeException](implicitly[Patcher[Entity]].patch(person, Seq(null, 'killer))).getMessage
      //tiny hack because Java 9 inserts the "java.base/" module name in the error message
      assert((msg eq null) || msg.startsWith("scala.Symbol cannot be cast to") && msg.endsWith("java.lang.Integer"))
    }

    "Inner Classes" - {
      class ParentClass {
        case class InnerClass(name: String)
        case class InnerClassWithDefault(name: String = "foo")

        def test1() = implicitly[Show[String, InnerClass]].show(InnerClass("foo"))
        def test2() = HasDefault.gen[InnerClassWithDefault].defaultValue
      }

      "serialize a case class inside another class" - {
        new ParentClass().test1() ==> "InnerClass(name=foo)"
      }

      "construct a default case class inside another class" - {
        val pc = new ParentClass()
        pc.test2() ==> Right(pc.InnerClassWithDefault())
      }
    }

    "Local Classes" - {
      def test1() = {
        case class LocalClass(name: String)
        implicitly[Show[String, LocalClass]].show(LocalClass("foo")) ==> "LocalClass(name=foo)"
      }

      def test2() = {
        case class LocalClassWithDefault(name: String = "foo")
        HasDefault.gen[LocalClassWithDefault].defaultValue ==> Right(LocalClassWithDefault())
      }

      "serialize a case class inside a method" - test1()

      "construct a default case class inside a method" - test2()
    }

    "show an Account" - {
      Show
        .gen[Account]
        .show(Account("john_doe", "john.doe@yahoo.com", "john.doe@gmail.com")) ==> "Account(id=john_doe,emails=[john.doe@yahoo.com,john.doe@gmail.com])"
    }

    "construct a default Account" - {
      HasDefault.gen[Account].defaultValue ==> Right(Account(""))
    }

    "construct a failed NoDefault" - {
      HasDefault.gen[NoDefault].defaultValue ==> Left("truth is a lie")
    }

    "show a Portfolio of Companies" - {
      Show
        .gen[Portfolio]
        .show(Portfolio(Company("Alice Inc"), Company("Bob & Co"))) ==> "Portfolio(companies=[Company(name=Alice Inc),Company(name=Bob & Co)])"
    }

    "show a List[Int]" - {
      Show
        .gen[List[Int]]
        .show(List(1, 2, 3)) ==> "::[Int](head=1,tl$access$1=::[Int](head=2,tl$access$1=::[Int](head=3,tl$access$1=Nil())))"
    }

    "sealed trait typeName should be complete and unchanged" - {
      TypeNameInfo.gen[Color].name.full ==> s"$testPackage.Color"
    }

    "case class typeName should be complete and unchanged" - {
      implicit val stringTypeName: TypeNameInfo[String] = new TypeNameInfo[String] {
        def name = ???
      }
      TypeNameInfo.gen[Fruit].name.full ==> s"$testPackage.Fruit"
    }

//    "show chained error stack" - {
//      scalac"""Show.gen[(Int, Seq[(Long, String)])]""" ==>
//      TypecheckError(txt"""magnolia: could not find Show.Typeclass for type Long
//      |    in parameter '_1' of product type (Long, String)
//      |    in chained implicit Show.Typeclass for type Seq[(Long, String)]
//      |    in parameter '_2' of product type (Int, Seq[(Long, String)])
//      |""")
//    }

    "show a recursive case class" - {
      Show.gen[Recursive].show(Recursive(Seq(Recursive(Nil)))) ==> "Recursive(children=[Recursive(children=[])])"
    }

    "manually derive a recursive case class instance" - {
      implicit lazy val showRecursive: Show[String, Recursive] = Show.gen[Recursive]
      showRecursive.show(Recursive(Seq(Recursive(Nil)))) ==> "Recursive(children=[Recursive(children=[])])"
    }

    "show a type aliased case class" - {
      type T = Person
      Show.gen[T].show(Person("Donald Duck", 313)) ==> "Person(name=Donald Duck,age=313)"
    }

    "dependencies between derived type classes" - {
      implicit def showDefaultOption[A](
          implicit showA: Show[String, A],
          defaultA: HasDefault[A]
      ): Show[String, Option[A]] = (optA: Option[A]) ⇒ showA.show(optA.getOrElse(defaultA.defaultValue.right.get))

      Show.gen[Path[String]].show(OffRoad(Some(Crossroad(Destination("A"), Destination("B"))))) ==>
      "OffRoad[String](path=Crossroad[String](left=Destination[String](value=A),right=Destination[String](value=B)))"
    }

    "capture attributes against params" - {
      Show
        .gen[Attributed]
        .show(Attributed("xyz", 100)) ==> "Attributed{MyAnnotation(0)}(p1{MyAnnotation(1)}=xyz,p2{MyAnnotation(2)}=100)"
    }

    "capture attributes against subtypes" - {
      Show
        .gen[AttributeParent]
        .show(Attributed("xyz", 100)) ==> "{MyAnnotation(0)}Attributed{MyAnnotation(0)}(p1{MyAnnotation(1)}=xyz,p2{MyAnnotation(2)}=100)"
    }

//    "show underivable type with fallback" - {
//      TypeNameInfo.gen[NotDerivable].name ==> TypeName("", "Unknown Type", Seq.empty)
//    }

//    "allow no-coproduct derivation definitions" - {
//      scalac"""WeakHash.gen[Person]""" ==> Returns(fqt"magnolia.examples.WeakHash[magnolia.tests.Person]")
//    }

//    "disallow coproduct derivations without dispatch method" - {
//      scalac"""WeakHash.gen[Entity]""" ==>
//      TypecheckError(
//        "magnolia: the method `dispatch` must be defined on the derivation object WeakHash to derive typeclasses for sealed traits")
//    }
  }
}
