BORER
=====

A [CBOR] and [JSON] (de)serialization implementation in [Scala] sporting these features:

- complete (supports all [CBOR] features, incl. "over long" integers, 16-bit half-precision floats, `BigInteger` and
  `BigDecimal`, custom tags and "simple values")
- lightweight (zero external dependencies)
- fast (no DOM, pull-parser style with one-element look-ahead)
- easy integration and customization (type class-based design)
- efficiently supports custom "byte string" abstractions (like `akka.util.ByteString` or `scodec.bits.ByteVector`)
- [scala.js] support

Apart from [CBOR] _BORER_ also supports high-performance de- and encoding from and to [JSON] through the same API.
One example where this is useful is providing "bilingual" REST APIs that can consume and produce both [CBOR] and [JSON].
_BORER_ makes this very easy.  

---

[![Maven Central](https://img.shields.io/maven-central/v/io.bullet/borer-core_2.12.svg)](https://maven-badges.herokuapp.com/maven-central/io.bullet/borer-core_2.12)
[![Uses Badges](https://img.shields.io/badge/uses-badges-ff69b4.svg)](http://shields.io/)


Basic Design Principles and Limitations 
---------------------------------------

_BORER_'s goal is to provide a highly efficient (de)serialization layer between some data model defined in Scala and
[CBOR] and/or [JSON] as storage/network formats. _BORER_ performs this task in the most direct way, without relying on
some Abstract Syntax Tree (AST, also called Document Object Model (DOM)) as intermediate structure.

As such it doesn't offer any facilities for pre- or post-processing the serialized data, like manipulating the [JSON]
structure, filtering or augmenting nodes, or the like.

Also, it doesn't rely on reflection in any way. All information about the types to encode and decode must be statically
available at the encoding/decoding point. With the exception of `sealed` ADT hierarchies (which are supported out of
the box) this means that you need to define yourself, how to represent type information of abstract types on the wire!

Another design principle has been to implement _BORER_'s core module without relying on Scala macros or depending on any
external libraries. This should make _BORER_ easily maintainable for the foreseeable future and reduces its weight as
a dependency of your applications (which can be especially important with [scala.js]).
(Note: The `borer-derivation` module _does_ rely on macros for deriving encoder/decoder type classes, but its use is
completely optional.)  


Modules
-------

_BORER_ consists of these modules:

- `borer-core`, no dependencies
- `borer-derivation`, (semi-)automatic codec derivation for case classes and ADTs, depends on [Magnolia]
- `borer-compat-akka`, support for `akka.util.ByteString`, depends on [akka-actor]
- `borer-compat-scodec`, support for `scodec.bits.ByteVector`, depends on [scodec]

Additionally, as a dependency of `borer-derivation`, _BORER_ also contains a `borer-magnolia` module, which contains an
internalized, patched copy of [Magnolia].    


Installation
------------

The artifacts for _BORER_ live on Maven Central and can be tied into your SBT-based Scala project like this:

```scala
libraryDependencies += "io.bullet" %% "borer-core" % "<version>"
libraryDependencies += "io.bullet" %% "borer-derivation" % "<version>" // for (semi-)automatic codec derivation
libraryDependencies += "io.bullet" %% "borer-compat-akka" % "<version>" // for direct `akka.util.ByteString` support
libraryDependencies += "io.bullet" %% "borer-compat-scodec" % "<version>" // for direct `scodec.bits.ByteVector` support
```

_BORER_ is available for [Scala] 2.12, 2.13.0-RC1 as well as [scala.js] (0.6 for Scala 2.12 and 1.0.0-M7 for Scala 2.13).


Basic Usage
-----------

Encoding a value to a plain `Array[Byte]`:

```scala
import io.bullet.borer.Cbor

val value = List("foo", "bar", "baz") // example value

val bytes: Array[Byte] =
  Cbor.encode(value).toByteArray // throws on error
```

Decoding a plain `Array[Byte]` back to a certain type:

```scala
import io.bullet.borer.Cbor

val list: List[String] =
  Cbor.decode(bytes).to[List[String]].value // throws on error
```

If you don't want _BORER_ to throw exceptions you can use the following variants to give you a `Try` instead:

```scala
val encoded: Try[Array[Byte]] =
  Cbor.encode(value).toByteArrayTry
```

and

```scala
val decoded: Try[List[String]] =
  Cbor.decode(bytes).to[List[String]].valueTry
```

Or, if you prefer encoding/decoding to an `Either` instance:

```scala
import io.bullet.borer.{Cbor, Output}

val encoded: Either[Cbor.Error[Output], Output] =
  Cbor.encode(value).bytesEither
```

and

```scala
import io.bullet.borer.{Cbor, Input}

val decoded: Either[Cbor.Error[Input], (List[String], Input)] =
  Cbor.decode(bytes).to[List[String]].valueEither
```

Check the sources of the central _BORER_ API entry point [here][Borer Source] for more info.
 


Types Supported Out-of-the-Box
------------------------------

_BORER_ comes with built-in encoding and decoding support for arbitrary combinations of these types:

- `Boolean`, `Char`, `Byte`, `Short` `Int`, `Long`, `Float`, `Double` and their boxed counterparts
- `Null`
- `String`
- `Array[Byte]` 
- `java.math.BigInteger`, `java.math.BigDecimal` and their scala wrappers `BigInt` and `BigDecimal`
- `Option[T]` 
- `Array[T]` 
- `M[T] <: Iterable[T]` 
- `M[A, B] <: Map[A, B]`
- `Iterator[T]`  (encoding only!)
- `Either[A, B]`
- `Tuple1[A]` ... `Tuple22[A, B, ... V]`

All these type are encoded to exactly one [CBOR] (or [JSON]) data item (which may of course be an array or map
consisting of other, nested data items.)


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
import io.bullet.borer.{Encoder, Decoder, Codec}

case class Color(name: String, value: Int)

implicit val encoder = Encoder.from(Color.unapply _)
implicit val decoder = Decoder.from(Color.apply _)

// alternative: provide an Encoder and Decoder at the same time
implicit val codec = Codec[T](Color.unapply _, Color.apply _)

// alternative: sugar for the above 
implicit val enc = Encoder.forCaseClass[Color] 
implicit val dec = Decoder.forCaseClass[Color]

// or simply
implicit val codec = Codec.forCaseClass[Color]
```

The codecs created in this way always encode a case class instance to a single [CBOR]/[JSON] data item: an array with
the length corresponding to the case classes arity and the member encodings forming the array elements.\
There is one exception though: In order to increase encoding efficiency unary case classes, with only one parameter,
have their single member written directly, without a wrapping single-element array.

If you would like your case classes to be encoded in a more JSON-esque way, as maps with each member being keyed by its
member name, check out the `borer-derivation` module [described below](#derivation).         


### Transforming Existing Encoders / Decoders

If your type is not a case class but can somehow be constructed from or deconstructed to any available Encoder or
Decoder respectively, you can rely on the `contramap` and `map` methods available on Encoders / Decoders:

```scala
import io.bullet.borer.{Encoder, Decoder}

class Person(name: String)

// have `Person` be encoded as a simple CBOR text data item 
implicit val encoder = Encoder.forString.contramap[Person](_.name)
implicit val decoder = Decoder.forString.map(Person(_))
```


### "Manual" Construction

For full flexibility of how your type `T` is to be encoded in [CBOR] you can of course also write the respective
`Encoder[T]` / `Decoder[T]` manually. This is done by explicitly defining how your type is to be written to a
`Writer` and read from a `Reader`:

```scala
import io.bullet.borer.{Encoder, Decoder}

class Person(name: String)

implicit val encoder: Encoder[Person] = Encoder((writer, person) => writer.writeString(person.name))
implicit val decoder: Decoder[Person] = Decoder(reader => Person(reader.readString()))
``` 

On the encoding side the `Writer` gives you a number of different methods for writing [CBOR] primitives,
while the `Reader` offers their counterparts on the decoding side.
The [next section](#reader-and-writer) has some more details on how to work with these two types. 
 
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

While _BORER_ (by default) verifies that the [CBOR]/[JSON] created by your application is indeed valid and will thus
catch any mistakes you made in this regard eventually, debugging structural problems can be a bit tedious since the
error will often only be recognizable at the very end of the encoding or decoding process. Check out the section on
[Logging](#Logging) below for more info how _BORER_ can support you in debugging (de)serialization issues.


#### Reader and Writer

All pre-defined Encoders and Decoders, as well as the ones you might write yourself, describe how to encode or decode
and object by operating on a `Writer` or `Reader`, respectively.

The `Writer` (sources [here][Writer Source]) contains a largish number of methods (like `writeInt`, `writeString` or
`writeArrayHeader`) that more or less directly write the respective data item to the output.
The `Reader` (sources [here][Reader Source]) contains the respective counterparts (like `readInt`, `readString` or
`readArrayHeader`).

The `Writer` and `Reader` operate directly on the respective `Output` and `Input` data "streams" and simply cause the
respective low-level "primitive" to be written or read. This means, that the logic working with them has to have at
least a basic understanding of the rules governing how these primitives can be or must be combined in order to produce
valid output.

For example, an "indefinite-length array" (in [CBOR] terminology) is written by first calling
`writer.writeArrayStart()`, then writing all the elements (recursively descending into any nested structures) and
finally "closing" the array with `writer.writeBreak()`.\

On the reading side this is mirrored by first having to call `reader.readArrayStart()`, then reading all the elements
(recursively decoding nested structures) and finally consuming the "closing" with `reader.readBreak()`.

While _BORER_ implements validation logic (enabled by default), which verifies the structural integrity of all produced
and consumed data, there are no static (type-level, i.e. compile-time) checks that catch you, when you forget to write
or read the BREAK primitive at the end!

When consuming [CBOR] data during decoding the `READER` gives you one-element look-ahead. This means that you can "see"
the kind of the next data item (primitive) that is available _before_ reading it, which is often very helpful.

For example, here is an potential `Decoder[Either[String, Int]]`:

```scala
import io.bullet.borer.Decoder

implicit val eitherStringIntDecoder: Decoder[Either[String, Int]] =
  Decoder { reader =>
    if (reader.hasString) Left(reader.readString())
    else if (reader.hasInt) Right(reader.readInt())
    else reader.unexpectedDataItem(expected = "`String` or `Int`")   
  }

```

`Input`, `Output`, `ByteAccess`
-------------------------------

In order to allow for seamless integration in all kinds of application environments _BORER_ abstracts over decoding
input, encoding output as well as general "chunks of bytes" with three additional type classes `Input[T]`, `Output[T]`
and `ByteAccess[T]`.


## Input

For decoding _BORER_ will happily consume any type for which an `Input.Wrapper[T]` is implicitly available,
which is responsible for constructing an `Input` around `T`.

Currently _BORER_ comes with predefined `Input` implementations for these types:

- `Array[Byte]`
- `java.nio.ByteBuffer`
- `akka.util.ByteString` (with the `borer-compat-akka` module)
- `scodec.bits.ByteVector` (with the `borer-compat-scodec` module)

The `Input` trait isn't particularly hard to implement, especially since it merely has to support single-pass access
to the underlying bytes with minimal buffering, no random access.   


## Output

On the encoding side _BORER_ can produce any type `T` for which an `Output.Provider[T]` is available,
which is responsible for producing an `Output` instance whose final output result has type `T`.

Currently _BORER_ comes with predefined `Output` implementations for these types:

- `Array[Byte]`
- `java.nio.ByteBuffer`
- `akka.util.ByteString` (with the `borer-compat-akka` module)
- `scodec.bits.ByteVector` (with the `borer-compat-scodec` module)

The `Output` trait isn't hard to implement as it simply writes out all bytes in a single, monotonic pass.


## ByteAccess

Unfortunately Scala (and the whole JVM eco-system) has no single, versatile abstraction for a "chunk of bytes" that
fits the needs of all applications. In order to remain open to the preferences of the application _BORER_ also
abstracts over "chunks of bytes" by allowing the use of any type `T`, for which a `ByteAccess[T]` is available.

Currently _BORER_ comes with predefined `ByteAccess` implementations for these types:

- `Array[Byte]`
- `java.nio.ByteBuffer`
- `akka.util.ByteString` (with the `borer-compat-akka` module)
- `scodec.bits.ByteVector` (with the `borer-compat-scodec` module)


JSON Support
------------

Since the [CBOR] data item primitives are a super set of what is available in [JSON], or, said differently, everything
in [JSON] has a counterpart in [CBOR], it's not hard for _BORER_ to also support encoding to and decoding from [JSON].

Here is how to encode a value to a plain `Array[Byte]` holding the UTF-8-encoded [JSON] output:

```scala
import io.bullet.borer.Json

val value = List("foo", "bar", "baz") // example value

val bytes: Array[Byte] = Json.encode(value).toByteArray // throws on error
```

Decoding a plain `Array[Byte]` holding UTF-8-encoded JSON input back to a certain type:

```scala
import io.bullet.borer.Json

val list: List[String] = Json.decode(bytes).to[List[String]].value // throws on error
```

The `io.bullet.borer.Json` object supports the same API as the `io.bullet.borer.Cbor` object.

From _BORER_'s point of view [JSON] is simply a slightly different binary format that only supports a subset of the
models data primitives. Like with [CBOR] _BORER_ encodes and decodes [JSON] in a *single pass*, UTF-8 encoding and
decoding to and from raw bytes on the fly.

All higher-level infrastructure (i.e. `Writer`, `Reader`, `Encoder`, `Decoder`, `Codec`, etc.) is essentially agnostic
to the (de)serialization target format. However, the `Writer` and `Reader` types do have a `target` member, which
enables custom logic to discriminate between the two variants, if required.\
Since the underlying [JSON] renderer will throw exceptions on attempts to write data primitives that are not supported
in [JSON] (like [CBOR] Tags), this is sometimes necessary to efficiently support both formats.

For example, in order to write an empty array in the most efficient way to both [CBOR] and [JSON] one would use this
approach:

```scala
import io.bullet.borer.Writer

def writeEmptyArray(w: Writer): w.type =
  if (writingJson) writeArrayStart().writeBreak()
  else writeArrayHeader(0) // fixed-sized Array Headers are not supported in JSON
```

However, as long as you don't use the `Reader` and `Writer` APIs to directly write low-level data primitives (like
Arrays and Maps), but simply construct your (de)serialization logic from the _BORER_'s built-in Encoders and Decoders,
your application should be able to support both [CBOR] and [JSON] at the same time without any special casing.


## JSON Performance

_BORER_ comes with a fast JSON parser, which enables _BORER_ to outperform most other popular JSON libraries in the
scala eco-system, when it comes to transforming JSON into a case-class based data model.\
A benchmark against [18 real-world JSON data examples][json-benchmark-files] from a diverse set of sources shows that
_BORER_'s JSON parsing infrastructure outperforms [Circe] by a factor of 1.3 - 2.9 (average: 1.71), the venerable
[spray-json] by a factor of 2.0 - 5.6 (average: 3.4) and even [Jackson Scala] by about 10 percent on average.

This is _despite_ BORER's external API being geared more towards user-friendliness rather than pure performance.
From a pure performance perspective a full type class-based design isn't ideal, since type classes can result a lot of
call sites becoming "megamorphic". Also, _BORER_ pull-style parser provides the user with one-element look-ahead, which
enables parsing into a DOM (if required), but, unfortunately, makes certain other performance optimizations unavailable.      
 
 
## Comparison with other Scala JSON Libraries

- [Circe]
  - DOM- and type class-based design 
  - PROS
    - very mature
    - allows for extensive DOM-manipulation
    - many integration option already available
    - compatible with [scala.js]
  - CONS
    - depends on `cats-core`
    - type class derivation can be slow (at compile time)
    - _BORER_ parses JSON about 1.7 times as fast
    - no [CBOR] support
- [spay-json]
  - DOM- and type class-based design 
  - PROS
    - zero dependencies
  - CONS
    - pre-historic, clunky type class design
    - essentially unmaintained
    - no support for case classes w/ more than 22 members
    - no type class derivation for ADTs
    - _BORER_ parses JSON about 3.4 times as fast
    - not compatible with [scala.js]    
    - no [CBOR] support
- [µPickle]
  - pull-style, type class-based design
  - PROS
    - zero dependencies
    - optional DOM 
    - also supports [MessagePack]
    - compatible with [scala.js]
  - CONS
    - no support for case classes w/ more than 22 members
    - _BORER_ parses JSON about twice as fast (on average)
    - no [CBOR] support
- [Jackson Scala]
  - Java implementation with a Scala add-on
  - PROS
    - very mature
    - good performance, especially when writing JSON 
  - CONS
    - no type class-based API
    - several non-Scala dependencies
    - not compatible with [scala.js]    
- [Jsoniter Scala]
  - pull-style, type class-based design
  - PROS
    - zero dependencies
    - very high performance (about twice as fast as _BORER_)
    - highly configurable
  - CONS
    - zero look-ahead API, DOM construction impossible
    - not compatible with [scala.js]
    - no [CBOR] support 

## When (not) to use _BORER_ for JSON

Since _BORER_ treats JSON as a binary format and reads/writes from/to raw bytes it isn't optimized for consuming or
producing Strings. (Strings have to first be UTF-8 encoded in order to be readable by _BORER_.)
So, if you need to frequently consume `String` input other [JSON] libraries will likely perform better.
Also, if you need to manipulate the JSON structures in any way between (de)serializing from/to the wire and from/to
your data model then _BORER_ will not help you and a DOM/AST-based JSON solution (like [Circe]) will likely be the
better choice.

However, if all you need is an efficient way to convert raw network- or disk-bytes holding UTF-8 encoded JSON to and
from your data model types, with no (or few) dependencies and maybe even with the option to target [CBOR] with no
additional work required from your side, then _BORER_ should be a good choice.      


Document Object Model (DOM)
---------------------------

While _BORER_'s core design is DOM-less, writing directly to and reading directly from the respective stream of
[CBOR]/[JSON] data items, it is sometimes convenient to nevertheless have access to an object structure that mirrors the
structure of a [CBOR] message as closely as possible. (Many JSON-libraries for example solely rely on such an "AST"
structure for their encoding and decoding operations.)

For such cases _BORER_ provides you with a simple "DOM" ADT (see the respective source file [here][Dom Source]),
which you can use like this:

```scala
import io.bullet.borer.Cbor
import io.bullet.borer.Dom._

val dom = MapElem.Sized(
  "foo" -> ArrayElem.Sized(IntElem(42), StringElem("rocks")),
  "bar" -> DoubleElem(26.8)
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
import io.bullet.borer.Cbor

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
    .value
``` 

produces this logging output to the console:

```
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
import io.bullet.borer.derivation.key
import io.bullet.borer.derivation.ArrayBasedCodecs._

sealed trait Animal
case class Dog(age: Int, name: String)                                        extends Animal
@key("TheCAT") case class Cat(weight: Double, color: String, home: String) extends Animal
@key(42) case class Mouse(tail: Boolean)                                   extends Animal

implicit val dogCodec = deriveCodec[Dog]
implicit val catCodec = deriveCodec[Cat]
implicit val mouseCodec = deriveCodec[Mouse]

implicit val animalCodec = deriveCodec[Animal]
```   

With these codecs case classes are written to [CBOR]/[JSON] in exactly the same fashion as with the case class support
in `borer-core` module (see [above](#case-classes)), i.e. to simple arrays (or the unwrapped member encoding if the case
classes has arity 1).

An Abstract Data Type (ADT) is encoded as a [CBOR] array of length two, with the first element holding the type id and
the second holding the instance's encoding (i.e. an array or single element).

The type id is required to allow the decoder to determine which ADT sub-type to decode into.
By default _BORER_ will use type's short class name as a (textual) type id.
If you want to customize this you can use the `@key` annotation to do so.
Check out the `@key` sources [here][key Source] for more info.


### Map-Based Codecs

Map-based codec derivation is enabled with this import:

```scala
import io.bullet.borer.derivation.MapBasedCodecs._
```

With these codecs case classes are encoded as [CBOR]/[JSON] maps with the member name as key, much as you would normally
expect a JSON codec to do it. 

ADTs are encoded as single-entry maps, with the key being the type id (`@key` or simple class name) and the value
becoming the encoding of the actual ADT subtype instance.


#### Default Values

Map-based codecs support missing and extra members. Extra members (i.e. map keys present in the encoding but not defined
as a case class member or `@key`) are simply being ignored.\
For missing members the type's `Decoder` will use the potentially defined default value, e.g.:

```scala
import io.bullet.borer.Json
import io.bullet.borer.derivation.MapBasedCodecs._

case class Dog(age: Int, name: String = "<unknown>")

implicit val dogCodec = deriveCodec[Dog]

Json
  .decode("""{ "age": 4 }""")
  .to[Dog]
  .value ==> Dog(age = 4) 
``` 

Also, `Encoder`/`Decoder` type classes can implement the `Encoder.DefaultValueAware` / `Decoder.DefaultValueAware`
trait in order to alter their behavior in the presence of a default value.

This is used, for example, by the pre-defined encoder and decoder for `Option[T]`, which change their encoding/decoding
strategy, if a `None` default value is defined for a case class member. In this case the optional value will only be
written if it's defined (and then without any wrapping structure). If the option is undefined nothing is written at all.
Correspondingly, during decoding the presence of the member yields a defined option instance holding the decoded value
while the member's missing in the encoding yields `None`.

This behavior matches the intution of what an `Option[T]` case class member would behave like when written to a [JSON]
representation. 


#### Customized Member Keys

_BORER_ supports customizing the name of case class members in the encoding with the `@key` annotation, that is also
used for custom ADT type-ids (see [above](#array-based-codecs)).
Simply annotate a case class member do provide a custom name:

```scala
import io.bullet.borer.Json
import io.bullet.borer.derivation.key

case class Dog(age: Int, @key("the-name")name: String)

implicit val dogCodec = deriveCodec[Dog]

Json.encode(Dog(1, "Lolle")).toUtf8String ==>
  """{"age":1,"the-name":"Lolle"}"""
```


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


Nullable and Default
--------------------

One question that frequently arises when dealing with [JSON], and to a limited extend [CBOR] as well, is: How to deal
with `null` values?\
`null` values differ from missing members (see also [Map-Based Codecs above](#map-based-codecs)) in that the value for
an element is indeed present, but it is `null`.

_BORER_ handles these case is a properly types fashion: If your data model allows for certain members to be `null` in
an encoding the members type should be wrapped with `Nullable`, i.e. `Nullable[String]`. In combination with the simple
type class `Default[T]`, which provides the capability to supply default values for a type `T` the pre-defined 
encoder and decoder for `Nullable[T]` will then be able to translate `null` values to respective default value and back.

Example:

```scala
import io.bullet.borer._

case class Dog(age: Int, name: Nullable[String])

implicit val dogCodec = deriveCodec[Dog]

Json
  .decode("""{ "age": 4, "name": null }""")
  .to[Dog]
  .value ==> Dog(age = 4, name = "") // the `Default[String]` provides an empty String 
```


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
  [JSON]: http://json.org/
  [Borer Source]: https://github.com/sirthias/borer/blob/master/core/src/main/scala/io/bullet/borer/Borer.scala 
  [akka-actor]: https://doc.akka.io/docs/akka/2.5/actors.html#dependency
  [Magnolia]: https://propensive.com/opensource/magnolia
  [scodec]: http://scodec.org/
  [MPL 2.0]: https://www.mozilla.org/en-US/MPL/2.0/
  [Cbor Source]: https://github.com/sirthias/borer/blob/master/core/src/main/scala/io/bullet/borer/Borer.scala
  [Writer Source]: https://github.com/sirthias/borer/blob/master/core/src/main/scala/io/bullet/borer/Writer.scala
  [Reader Source]: https://github.com/sirthias/borer/blob/master/core/src/main/scala/io/bullet/borer/Reader.scala
  [Dom Source]: https://github.com/sirthias/borer/blob/master/core/src/main/scala/io/bullet/borer/Dom.scala
  [TypeId Source]: https://github.com/sirthias/borer/blob/master/derivation/src/main/scala/io/bullet/borer/derivation/TypeId.scala
  [79]: https://github.com/propensive/magnolia/issues/79
  [114]: https://github.com/propensive/magnolia/issues/114
  [Circe]: https://circe.github.io/circe/
  [spray-json]: https://github.com/spray/spray-json/