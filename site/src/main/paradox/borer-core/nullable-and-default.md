Nullable and Default
====================

One question that frequently arises when dealing with [JSON], and to a limited extend [CBOR] as well, is how to deal
with `null` values.

`null` values differ from missing members (see also @ref:[Map-Based Codecs](../borer-derivation/map-based.md#default-values))
in that the value for an element is indeed present, but is `null`.

_borer_ handles this case in a properly typed fashion: If your data model allows for certain members to have a `null`
encoding that you would like to treat specially the member's type should be wrapped with `Nullable`, e.g.
`Nullable[String]`.<br>
In combination with the simple type class `Default[T]`, which provides the capability to supply default values for a
type `T`, the pre-defined Encoder and Decoder for `Nullable[T]` will be able to translate `null` values to the
respective default value and back.

Example:

@@snip [-]($test$/NullableSpec.scala) { #example }


`NullOptions`
-------------

Sometimes it's convenient to map "nullable" fields to Options, i.e. `null` to `None` and non-null to `Some`.
This can easily be done with this import:

```scala
import io.bullet.borer.NullOptions._
```

Here is how `NullOptions` are implemented:

@@snip [-]($core$/NullOptions.scala) { #docs-quote-delimiter }

@@@ note

At first glance it might seem that `NullOptions` could actually be the default way to encode a value of type
`Option[T]` since `null` appears as the "natural" construct in [JSON] / [CBOR] to represent "no value".<br>
However, `NullOptions` have a serious drawback which makes them unsuitable as the generally preferred representation
strategie for `Option[T]`:<br>They are _unsound_ as in "they don't compose".

Consider the type `Option[Option[T]]`. With `NullOptions` its value `Some(None)` would serialize to `null`, which would
then deserialize to `None` rather than `Some(None)`, which is _unsound_ as it violates the basic "roundtrip requirement"
`deserialize(serialize(x)) == x`.

@@@

  [CBOR]: http://cbor.io/
  [JSON]: http://json.org/