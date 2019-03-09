BORER
=====

A [CBOR] (de)serialization implementation in [Scala] sporting these features:

- complete (supports all [CBOR] features, incl. "over long" integers, 16-bit half-precision floats, `BigInteger` and
  `BigDecimal`, custom tags and "simple values")
- lightweight (`core` module has zero dependencies)
- fast (allocation-free in the core code paths, no DOM, pull-parser style)
- easy integration (type class-based design)
- efficiently supports custom "byte string" abstractions (like `akka.util.ByteString` or `scodec.bits.ByteVector`)

---

[![Maven Central](https://img.shields.io/maven-central/v/io.bullet/borer-core_2.12.svg)](https://maven-badges.herokuapp.com/maven-central/io.bullet/borer-core_2.12)
[![Uses Badges](https://img.shields.io/badge/uses-badges-ff69b4.svg)](http://shields.io/)


Modules
-------

_BORER_ consists of these modules:

- `borer-core`, no dependencies
- `borer-derivation`, (semi-)automatic codec derivation for case classes and ADTs, depends on [Magnolia]
- `borer-compat-akka`, support for `akka.util.ByteString`, depends on [akka-actor]
- `borer-compat-scodec`, support for `scodec.bits.ByteVector`, depends on [scodec]


Installation
------------

The artifacts for _BORER_ live on Maven Central and can be tied into your SBT-based Scala project like this:

```scala
libraryDependencies += "io.bullet" %% "borer-core" % "<version>"
libraryDependencies += "io.bullet" %% "borer-derivation" % "<version>" // for (semi-)automatic codec derivation
libraryDependencies += "io.bullet" %% "borer-compat-akka" % "<version>" // for direct `akka.util.ByteString` support
libraryDependencies += "io.bullet" %% "borer-compat-scodec" % "<version>" // for direct `scodec.bits.ByteVector` support
```

_BORER_ is available for [Scala] 2.12 as well as [scala.js] 0.6.


Basic Usage
-----------

Encoding a value to a plain `Array[Byte]`:

```scala
import io.bullet.borer.core.Cbor

val value = List("foo", "bar", "baz") // example value

val bytes: Array[Byte] = Cbor.encode(value).toByteArray // throws on error
```

Decoding a plain `Array[Byte]` back to a certain type:

```scala
import io.bullet.borer.core.Cbor

val list: List[String] = Cbor.decode(bytes).to[List[String]].value // throws on error
```

If you don't want _BORER_ to throw exceptions you can use the following variants to give you a `Try` instead:

```scala
val encoded: Try[Array[Byte]] = Cbor.encode(value).toByteArrayTry
```

and

```scala
val decoded: Try[List[String]] = Cbor.decode(bytes).to[List[String]].valueTry
```

Or, if you prefer encoding/decoding to an `Either` instance:

```scala
import io.bullet.borer.core.{Cbor, Output}

val encoded: Either[Cbor.Error[Output], Output] = Cbor.encode(value)
```

and

```scala
import io.bullet.borer.core.{Cbor, Input}

val decoded: Either[Cbor.Error[Input], (List[String], Input)] = Cbor.decode(bytes).to[List[String]]
```

Check the sources of the central _BORER_ API entry point [here][Cbor Source] for more info.
 


Types Supported Out-of-the-Box
------------------------------

_BORER_ comes with built-in encoding and decoding support for arbitrary combinations of these types:

- `Boolean`, `Char`, `Byte`, `Short` `Int`, `Long`, `Float`, `Double` and their boxed counterparts
- `Null`
- `String`
- `Array[Byte]` 
- `java.math.BigInteger`, `java.math.BigDecimal` and their scala wrappers `BigInt` and `BigDecimal`
- `Option[T]` 
- `Array[T <: AnyRef]` 
- `M[T] <: Iterable[T]` 
- `M[A, B] <: Map[A, B]`
- `Iterator[T]`  (encoding only!)
- `Either[A, B]`
- `Tuple1[A]` ... `Tuple22[A, B, ... V]`

All these type are encoded to exactly one [CBOR] data item (which may of course be an array or map consisting of other,
nested data items.)


Encoding and Decoding Custom Types
----------------------------------

In order to encode some custom type `T` you'll have to implicitly provide an `Encoder[T]`, which is defined like this:

```scala
trait Encoder[T] {
  def write(w: Writer, value: T): w.type
}
```

Similarly, for decoding of `T` you'll have to implicitly provide `Decoder[T]`, which is defined like this:

```scala
trait Decoder[T] {
  def read(r: Reader): T
}
```

Many times, when encoding _and_ decoding must be available for a type it's easier to supply just a single implicit for
`T`, rather than two. As an alternative to providing a separate `Encoder[T]` as well as a  `Decoder[T]`  you can also
provide a `Codec[T]`, which is defined like this:

```scala
final case class Codec[T](encoder: Encoder[T], decoder: Decoder[T])
```

Encoders and Decoders can be implicitly "unpacked" from a `Codec`.

*NOTE*: In order to not hinder composability Codecs should only ever be _supplied_, never consumed.
So, if you write an `Encoder`, `Decoder` or `Codec` for a generic type, which itself requires implicitly available
encoders and/or decoders for certain type parameters (like `Encoder.forOption`, for example) then you should never
require an implicitly available `Codec[T]`, but rather an `Encoder[T]` and `Decoder[T]` separately. 

There are several ways to provide such encoders, decoders or codecs for your custom types.\
The following sections outline the alternatives.


### Case Classes

If `T` is a case class then an `Encoder[T]` and/or `Decoder[T]` can be concisely provided by via the `unapply` / `apply`
methods of the `T` companion object: 

```scala
import io.bullet.borer.core.{Encoder, Decoder, Codec}

case class Color(name: String, value: Int)

implicit val encoder = Encoder.from(Color.unapply _)
implicit val decoder = Decoder.from(Color.apply _)

// alternative: provide an Encoder and Decoder at the same time
implicit val codec = Codec.of[T](Color.unapply _, Color.apply _)

// alternative: sugar for the above 
implicit val enc = Encoder.forCaseClass[Color] 
implicit val dec = Decoder.forCaseClass[Color]

// or simply
implicit val codec = Codec.forCaseClass[Color]
```

The codecs created in this way always encode a case class instance to a single [CBOR] data item: a [CBOR] array with
the length corresponding to the case classes arity and the member encodings forming the array elements.\
There is one exception though: In order to increase encoding efficiency unary case classes, with only one parameter,
have their single member written directly, without a wrapping single-element array.

If you would like your case classes to be encoded in a more JSON-esque way, as maps with each member being keyed by its
member name, check out the `borer-derivation` module [described below](#derivation).         


### Transforming Existing Encoders / Decoders

If your type is not a case class but can somehow be constructed from or deconstructed to any available Encoder or
Decoder respectively, you can rely on the `compose` and `map` methods available on Encoders / Decoders:

```scala
import io.bullet.borer.core.{Encoder, Decoder}

class Person(name: String)

// have `Person` be encoded as a simple CBOR text data item 
implicit val encoder = Encoder.forString.compose[Person](_.name)
implicit val decoder = Decoder.forString.map(Person(_))
```


### "Manual" Construction

For full flexibility of how your type `T` is to be encoded in [CBOR] you can of course also write the respective
`Encoder[T]` / `Decoder[T]` manually. This is done by explicitly defining how your type is to be written to a
`Writer` and read from a `Reader`:

```scala
import io.bullet.borer.core.{Encoder, Decoder}

class Person(name: String)

implicit val encoder: Encoder[Person] = Encoder((writer, person) => writer.writeString(person.name))
implicit val decoder: Decoder[Person] = Decoder(reader => Person(reader.readString()))
``` 

On the encoding side the `Writer` gives you a number of different methods for writing [CBOR] primitives,
while the `Reader` offers their counterparts on the decoding side.
Check out the sources of these types (`Writer` [here][Writer Source] and `Reader` [here][Reader Source]).
Hopefully their APIs are somewhat self-explanatory. 
 
While this low-level way of defining the encoding/decoding logic is the most powerful it also requires a little more
care.\
For performance reasons both the `Writer` and `Reader` types are mutable abstractions, which means that the order
in which you call their methods matters a lot. 

Also, very importantly, when deciding on an encoding logic for any type (i.e. how to represent the type with the
available [CBOR] primitives) make sure to always encode it to exactly **one** [CBOR] data item! (Unless you know
exactly, what you are doing.) All built-in encoders and decoders, e.g. for case classes, arrays, maps, etc., always
assume that any object is written to exactly data item.\
So, if you need to write several values, wrap them in an array or map! And rather than writing no value at all write
some kind of placeholder, like `null`, `undefined` or an empty array or map!

To illustrate the point: The default codec for `Option[T]` for example encodes `Some[T]` to a single element array
holding the encoding of `T`, and `None` to a zero-element (empty) array!

While _BORER_ (by default) verifies that the [CBOR] created by your application is indeed valid and will thus catch any
mistakes you made in this regard eventually, debugging structural problems can be a bit tedious since the error will
often only be recognizable at the very end of the encoding or decoding process. Check out the section on
[Logging](#Logging) below for more info how _BORER_ can support you in debugging (de)serialization issues.


Document Object Model (DOM)
---------------------------

While _BORER_'s core design is DOM-less, writing directly to and reading directly from the respective stream of [CBOR]
data items, it is sometimes convenient to nevertheless have access to an object structure that mirrors the structure
of a [CBOR] message as closely as possible. (Many JSON-libraries for example solely rely on such an "AST" structure for
their encoding and decoding operations.)

For such cases _BORER_ provides you with a simple "DOM" ADT (see the respective source file [here][Dom Source]),
which you can use like this:

```scala
import io.bullet.borer.core.Cbor
import io.bullet.borer.core.Dom.Element

val dom = Element.Map(
  "foo" -> Element.Array(Element.Value.Int(42), Element.Value.String("rocks")),
  "bar" -> Element.Value.Double(26.8)
)  

val encoded = Cbor.encode(dom).toByteArray
val decoded = Cbor.decode(encoded).to[Element].value
```   


Logging
------- 

With [CBOR] being a binary format debugging problems can sometimes be a bit more difficult than with text-based formats
like JSON, which are immediately human-readable.

In order to help debug problems with encoding or decoding, independently of whether they are caused by your own
application code or invalid / unexpected input, you can easily switch on logging of the encoding or decoding process
with the `.withPrintLogging()` modifier.

For example, this snippet:

```scala
import io.bullet.borer.core.Cbot

val value =
  Map(
    "foo" → Left(42),
    "bar" → Right(List("For", "the", "King!"))
  )
val encoded = Cbor.encode(value).toByteArray

val decoded = 
  Cbor
    .decode(encoded)
    .withPrintLogging() // just insert this line to enable logging
    .to[Map[String, Either[Int, List[String]]]]
``` 

produces this logging output to the console:

```
RESET
1: {
    1/2: "foo"
    1/2: -> {
        1/1: 0
        1/1: -> 42
    1/2: }
    2/2: "bar"
    2/2: -> {
        1/1: 1
        1/1: -> [
            1/3: "For"
            2/3: "the"
            3/3: "King!"
        1/1: ]
    2/2: }
1: }
2: END
```

which makes it very easy to see, what exactly the decoding input consists of.
Also, in case of any exception the log will break off exactly at the point where the exception got thrown,
which is often a great help in locating the problem.   


Derivation
----------

The `borer-core` module by itself only provides relatively limited support for case classes and no support for
sealed Abstract Data Types (ADT) hierarchies.

When you include the `borer-derivation` module as a dependency (see *Installation* above) _BORER_ can
(semi-automatically) provide encoders and decoders for case classes and ADTs by deriving them with the help of
[Magnolia].

There are two basic alternatives to choose from: *Array-Based Codecs* or *Map-Based Codecs*.


### Array-Based Codecs

Array-Based Codec derivation is enabled with this import:

```scala
import io.bullet.borer.derivation.ArrayBasedCodecs._
```

This brings the three methods `deriveEncoder[T]`, `deriveDecoder[T]` and `deriveCodec[T]` in scope,
which you can use to derive the respective type classes for case classes and ADTs.

Here is an example:

```scala
import io.bullet.borer.derivation.TypeId
import io.bullet.borer.derivation.ArrayBasedCodecs._

sealed trait Animal
case class Dog(age: Int, name: String)                                        extends Animal
@TypeId("TheCAT") case class Cat(weight: Double, color: String, home: String) extends Animal
@TypeId(42) case class Mouse(tail: Boolean)                                   extends Animal

implicit val dogCodec = deriveCodec[Dog]
implicit val catCodec = deriveCodec[Cat]
implicit val mouseCodec = deriveCodec[Mouse]

implicit val animalCodec = deriveCodec[Animal]
```   

With these codecs case classes are written to [CBOR] in exactly the same fashion as with the case class support in
`borer-core` module (see [above](#case-classes)), i.e. to simple arrays (or the unwrapped member encoding if the case
classes has arity 1).

An Abstract Data Type (ADT) is encoded as a [CBOR] array of length two, with the first element holding the type id and
the second holding the instance's encoding (i.e. an array or single element).

The type id is required to allow the decoder to determine which ADT sub-type to decode into.
By default _BORER_ will use type's short class name as a (textual) type id.
If you want to customize this you can use the `@TypeId` annotation to do so.
Check out the `@TypeId` sources [here][TypeId Source] for more info.


### Map-Based Codecs

Map-Based Codec derivation is enabled with this import:

```scala
import io.bullet.borer.derivation.MapBasedCodecs._
```

With these codecs case classes are encoded as [CBOR] maps with the member name as key, much as you would normally expect
a JSON codec to do it. 

ADTs, however, are encoded in exactly the same way as with the Array-Based Codecs described above, i.e. as two-element
arrays with the type id as first element and the map as second.


### Derivation Gotchas

Since _BORER_ derivation relies on [Magnolia] for doing the dirty macro work behind the scenes there are a few gotchas
to be aware off.

First, _BORER_ derivation is intentionally not fully automatic, i.e. codecs are not created implicitly but require
you to write one line per type. The best-practice here is to cache the created codec in an `implicit val` or
`implicit lazy val` (in case there are circular dependencies between the types).

Without this explicit caching (i.e. when [Magnolia] provides the codecs implicitly, on demand) the code created for
recursive ADT hierarchies can quickly become enormous and make compilation as well as runtime unusually slow or even
fail. (See Magnolia issues [79] and [114] for more info.)

When you cache the individual codecs in their own respective `val`s (or `lazy val`s) [Magnolia]'s derivation is fast
and efficient.


Akka Support
------------

The `borer-core` module by itself only knows how to encode to and decode from plain byte arrays (`Array[Byte]`).
When you include the `borer-compat-akka` module as a dependency (see *Installation* above) and

```scala
import io.bullet.borer.compat.akka._
```

you also get full "zero-copy" support for encoding to and decoding from `akka.util.ByteString` as well as an implicit
`Encoder[ByteString]` and `Decoder[ByteString]`.    


Scodec Support
--------------

The `borer-core` module by itself only knows how to encode to and decode from plain byte arrays (`Array[Byte]`).
When you include the `borer-compat-scodec` module as a dependency (see *Installation* above) and

```scala
import io.bullet.borer.compat.scodec._
```

you also get full "zero-copy" support for encoding to and decoding from `scodec.bits.ByteVector` as well as an implicit
`Encoder[ByteVector]` and `Decoder[ByteVector]`.


License
-------

_BORER_ is released under the [MPL 2.0].

Contributions are always welcome!  


  [Scala]: https://www.scala-lang.org/
  [scala.js]: https://www.scala-js.org/
  [CBOR]: http://cbor.io/
  [akka-actor]: https://doc.akka.io/docs/akka/2.5/actors.html#dependency
  [Magnolia]: https://propensive.com/opensource/magnolia
  [scodec]: http://scodec.org/
  [MPL 2.0]: https://www.mozilla.org/en-US/MPL/2.0/
  [Cbor Source]: https://github.com/sirthias/borer/blob/master/core/src/main/scala/io/bullet/borer/core/Cbor.scala
  [Writer Source]: https://github.com/sirthias/borer/blob/master/core/src/main/scala/io/bullet/borer/core/Writer.scala
  [Reader Source]: https://github.com/sirthias/borer/blob/master/core/src/main/scala/io/bullet/borer/core/Reader.scala
  [Dom Source]: https://github.com/sirthias/borer/blob/master/core/src/main/scala/io/bullet/borer/core/Dom.scala
  [TypeId Source]: https://github.com/sirthias/borer/blob/master/derivation/src/main/scala/io/bullet/borer/derivation/TypeId.scala
  [79]: https://github.com/propensive/magnolia/issues/79
  [114]: https://github.com/propensive/magnolia/issues/114