Encoding and Decoding Custom Types
==================================

In order to encode some custom type `T` you'll have to implicitly provide an `Encoder[T]`:

```scala
trait Encoder[T] {
  def write(w: Writer, value: T): w.type
}
```

Similarly, for decoding of `T` you'll have to implicitly provide `Decoder[T]`:

```scala
trait Decoder[T] {
  def read(r: Reader): T
}
```

Many times, when encoding _and_ decoding must be available for a type, it's easier to supply just a single implicit for
`T`, rather than two. As an alternative to providing a separate `Encoder[T]` as well as a  `Decoder[T]`  you can also
provide a `Codec[T]`, which is defined like this:

```scala
final case class Codec[T](encoder: Encoder[T], decoder: Decoder[T])
```

Encoders and Decoders can be implicitly "unpacked" from a `Codec`.

There are several ways to provide such encoders, decoders or codecs for your custom types.<br>
The following sections outline the alternatives.

@@@ note

In order to not hinder composability Codecs should only ever be _supplied_, never consumed.<br>
So, if you write an `Encoder`, `Decoder` or `Codec` for a generic type, which itself requires implicitly available
encoders and/or decoders for certain type parameters (like `Encoder.forOption`, for example) then you should never
require an implicitly available `Codec[T]`, but rather an `Encoder[T]` and `Decoder[T]` separately.

For example, you should say:

```scala
// ok: require `Encoder`/`Decoder` instances and supply a `Codec`
implicit def forOption[T: Encoder :Decoder]: Codec[Option[T]] = ...
```

rather than

```scala
// bad: requiring a `Codec` (here: as a Context Bound)
implicit def forOption[T: Codec]: Codec[Option[T]] = ...
```

@@@
 

Case Classes
------------

The best way to concisely generate codecs for case classes is _borer_'s @ref:[Derivation](../borer-derivation/index.md)
module.

If for some reason you cannot or don't want to use macro-based codec derivation and array-based codecs are sufficient
for you use case then there is a macro-less solution available with the `borer-core` alone:

If `T` is a case class with at most 22 members then an `Encoder[T]` and/or `Decoder[T]` can be concisely provided by via
the `unapply` / `apply` methods of the `T` companion object: 

@@snip [-]($test$/CustomCodecsSpec.scala) { #from-unapply-apply }

The codecs created in this way always encode a case class instance to a single [CBOR]/[JSON] data item: an array with
the length corresponding to the case classes arity and the member encodings forming the array elements.<br>
There is one exception though: In order to increase encoding efficiency unary case classes, with only one parameter,
have their single member written directly, without a wrapping single-element array.

The encoding strategy is thus identical to the one produced by the macro-derived
@ref:[ArrayBasedCodecs](../borer-derivation/01-array-based.md).

If you would like your case classes to be encoded in a more JSON-esque way, as maps with each member being keyed by its
member name, the @ref:[MapBasedCodecs](../borer-derivation/02-map-based.md) from the
@ref:[`borer-derivation`](../borer-derivation/index.md) module are your only option, short of
@ref:[Manual Construction](#construction).           


Transforming Existing Encoders / Decoders
-----------------------------------------

If your type can somehow be constructed from or deconstructed to another type that an Encoder or Decoder is already
available for, you can rely on the `contramap` and `map` methods available on Encoders / Decoders:

@@snip [-]($test$/CustomCodecsSpec.scala) { #map-contramap }


"Manual" Construction
---------------------

For full flexibility of how your type `T` is to be encoded in [CBOR]/[JSON] you can of course also write the respective
`Encoder[T]` / `Decoder[T]` manually. This is done by explicitly defining how your type is to be written to a
`Writer` and read from a `Reader`:

@@snip [-]($test$/CustomCodecsSpec.scala) { #manual } 

On the encoding side the `Writer` gives you a number of different methods for writing [CBOR]/[JSON] primitives,
while the `Reader` offers their counterparts on the decoding side.
The @ref:[next section](#reader-and-writer) has some more details on how to work with these two types. 
 
While this low-level way of defining the encoding/decoding logic is the most powerful it also requires a little more
care.<br>
For performance reasons both the `Writer` and `Reader` types are mutable abstractions, which means that the order
in which you call their methods matters a lot.

@@@ warning { title=Caution }

Also, very importantly, when deciding on an encoding logic for any type (i.e. how to represent the type with the
available [CBOR]/[JSON] primitives) make sure to always encode it to exactly **one** data item! (Unless you know
exactly, what you are doing.) All built-in encoders and decoders, e.g. for case classes, arrays, maps, etc., always
assume that any object is written to exactly data item.

So, if you need to write several values, wrap them in an array or map! And rather than writing no value at all write
some kind of placeholder, like `null`, `undefined` or an empty array or map!

@@@

To illustrate the point: The default codec for `Option[T]` for example encodes `Some[T]` to a single element array
holding the encoding of `T`, and `None` to a zero-element (empty) array.

While _borer_ (by default) verifies that the [CBOR]/[JSON] created by your application is indeed valid and will thus
catch any mistakes you made in this regard eventually, debugging structural problems can be a bit tedious since the
error will often only be recognizable at the very end of the encoding or decoding process. Check out the section on
@ref:[Debugging](10-debugging.md) for more info how _borer_ can support you in debugging (de)serialization issues.


### Reader and Writer

All pre-defined Encoders and Decoders, as well as the ones you might write yourself, describe how to encode or decode
and object by operating on a `Writer` or `Reader`, respectively.

The `Writer` (sources @github[here](/core/src/main/scala/io/bullet/borer/Writer.scala)) contains a largish number of
methods (like `writeInt`, `writeString` or `writeArrayHeader`) that more or less directly write the respective data item
to the output.<br>
The `Reader` (sources @github[here](/core/src/main/scala/io/bullet/borer/Reader.scala)) contains the respective
counterparts (like `readInt`, `readString` or `readArrayHeader`).

The `Writer` and `Reader` operate directly on the respective `Output` and `Input` data "streams" and simply cause the
respective low-level "primitive" to be written or read. This means, that the logic working with them has to have at
least a basic understanding of the rules governing how these primitives can be or must be combined in order to produce
valid output.

For example, an "indefinite-length array" (in [CBOR] terminology) is written by first calling
`writer.writeArrayStart()`, then writing all the elements (recursively descending into any nested structures) and
finally "closing" the array with `writer.writeBreak()`.

On the reading side this is mirrored by first having to call `reader.readArrayStart()`, then reading all the elements
(recursively decoding nested structures) and finally consuming the "closing" with `reader.readBreak()`.

While _borer_ implements validation logic (enabled by default), which verifies the structural integrity of all produced
and consumed data, there are no static (type-level, i.e. compile-time) checks that catch you, when you forget to write
or read the BREAK primitive at the end!

When consuming [CBOR] data during decoding the `Reader` gives you one-element look-ahead. This means that you can "see"
the kind of the next data item (primitive) that is available _before_ reading it, which is often very helpful.

For example, here is an potential `Decoder[Either[String, Int]]`:

@@snip [-]($test$/CustomCodecsSpec.scala) { #lookahead }

  [CBOR]: http://cbor.io/
  [JSON]: http://json.org/