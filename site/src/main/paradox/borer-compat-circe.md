`borer-compat-circe`
====================

The `borer-compat-circe` module allows existing serialization code written against [circe] to be re-used for [CBOR][CBOR]
(de)serialization with minimal effort. 


Background
----------

[circe] is a mature [JSON] library that's quite popular throughout the Scala ecosystem.<br>
Contrary to _borer_ [circe][circe] (de)serializes [JSON] not directly to/from the application-level model classes but
models [JSON] documents with an intermediate-level AST (Abstract Syntax Tree) or DOM (Document Object Model).<br>
As shown in Figure 1 the `Encoder` / `Decoder` logic you write (or that [circe] derives for you) merely translates
between your own types and this JSON AST/DOM.

```raw
<figure>
  <img src="assets/images/schema-circe.svg" alt="circe schematic">
  <figcaption>Figure 1. <i>circe</i> schematic</figcaption>
</figure>
```

_borer_ on the other hand translates directly between your application-level model classes and the [JSON] document,
without going through an intermediate AST/DOM representation (Figure 2).

```raw
<figure>
  <img src="assets/images/schema-borer.svg" alt="borer schematic">
  <figcaption>Figure 2. <i>borer</i> schematic</figcaption>
</figure>
```

The `borer-compat-circe` module provides you with _borer_ `Encoder` and `Decoder` type classes for [circe]'s AST node
types, which allows you to combine both libraries as shown in Figure 3. 

```raw
<figure>
  <img src="assets/images/schema-circe-compat.svg" alt="borer-circe-compat schematic">
  <figcaption>Figure 3. <i>borer-circe-compat</i> schematic</figcaption>
</figure>
```

The benefit of this construct is that existing encoding/decoding logic that so far has been only targeting [JSON]
via [circe] can now also be used to target [CBOR] through _borer_.<br>
(Theoretically you could also use _borer_ to target [JSON] with this construct but there wouldn't be much point in doing
so as [circe] can of course read and write its own AST nodes to and from [JSON] without _borer_'s help. Also, due to
optimal integration between the layers, [circe] can likely do the job more efficiently that any external library ever
could.)


Usage
-----

When you include the `borer-compat-circe` module as a dependency (see the @ref:[Getting Started](getting-started.md)
chapter for details) you can write code such as this:

@@snip [-]($test$/CirceCompatSpec.scala) { #example }


Limitations
-----------

Since [JSON] is merely a subset of [CBOR] and, as such, there are constructs in [CBOR] that do not directly map onto
[JSON] not all [CBOR] documents can be easily decoded via a [JSON] deserialization layer such as the one provided
by [circe]. 

Most importantly the following [CBOR] constructs do not readily and easily map onto [JSON]:

`undefined`
: By default `undefined` values are decoded as `null` values.

Raw Byte Strings
: By default raw byte strings are [base64]-encoded and passed to [circe] as [JSON] strings. 

Custom Simple Values
: By default an exception is thrown upon reading a custom [CBOR] "simple value". 

The behavior of the `borer-circe-compat` decoding logic can be customized, if necessary, by constructing the _borer_
`Decoder[io.circe.Json]` with a custom call to `io.bullet.borer.compat.circe.circeJsonAstDecoder(...)`.

See the @github[module sources](/compat-circe/src/main/scala/io/bullet/borer/compat/circe.scala#L90) for full details.

  [circe]: https://circe.github.io/circe/
  [CBOR]: http://cbor.io/
  [JSON]: http://json.org/
  [base64]: https://tools.ietf.org/html/rfc4648#section-4