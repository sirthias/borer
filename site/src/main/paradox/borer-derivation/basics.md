Derivation Basics
=================

In order to have _borer_ automatically derive Encoders and/or Decoders for your custom types you need to first include
the `borer-derivation` module as a dependency.<br>
See the @ref:[Getting Started](../getting-started.md) chapter for details.

Once that is done you need to pick one of two available encoding strategies: 

- @ref:[Array-Based Codecs](array-based.md) or
- @ref:[Map-Based Codecs](map-based.md).

As the name implies the difference between these two encoding strategies lies in the type of the underlying structure
that class instances are serialized to.

The @ref:[Array-Based Codecs](array-based.md) render class instances into
arrays where each member is identified merely via its positional index. This is more efficient than
@ref:[Map-Based](map-based.md) encoding, with regard to both runtime performance and storage requirements.

The @ref:[Map-Based Codecs](map-based.md) on the other hand render class members as named map entries, which has the
the benefit that the wire format is more descriptive, not dependent on ordering and even allows for missing and default
members.


Semi-Automatic Derivation
-------------------------

Once you have the respective import for @ref:[Array-Based](array-based.md) or @ref:[Map-Based Codecs](map-based.md)
derivation in scope the following macros for semi-automatic derivation are available:
 
- `deriveEncoder[T]`, which derives an `Encoder[T]`
- `deriveDecoder[T]`, which derives a `Decoder[T]`
- `deriveCodec[T]`, which derives a `Codec[T]`, i.e. an `Encoder[T]` as well as a `Decoder[T]`

These methods work for all `T` that are are either `case class`es, `sealed trait`s or `sealed abstract class`es.

See the chapters on @ref:[Array-Based Codecs](array-based.md) or @ref:[Map-Based Codecs](map-based.md) for examples.


Fully-Automatic Derivation
--------------------------

In addition to semi-automatic derivation, which will only ever derive a typeclass instance for a single type, _borer_
also provides the following macros for fully-automatic derivation of encoder/decoder typeclasses for whole ADT
hierarchies, i.e. `sealed trait`s or `sealed abstract class`es and all their sub-types:

- `deriveAllEncoders[T]`, which derives an `Encoder[T]`
- `deriveAllDecoders[T]`, which derives a `Decoder[T]`
- `deriveAllCodecs[T]`, which derives a `Codec[T]`, i.e. an `Encoder[T]` as well as a `Decoder[T]`

To understand what these macros do consider this simple example:

@@snip [-]($test$/DerivationSpec.scala) { #example-adt }

With semi-automatic derivation you'd have to explicitly call a derivation macro _for each_ sub-type individually:

@@snip [-]($test$/DerivationSpec.scala) { #adt-semi-automatic-codec-derivation }

This gives you full control but can be a bit tedious for larger ADT hierarchies.

Fully-automatic derivation on the other hand lets you reduce the boilerplate to a minimum by generating the calls
to the derivation macro for each ADT sub-type automatically, so you only need to write a single line:   

@@snip [-]($test$/DerivationSpec.scala) { #adt-fully-automatic-codec-derivation }


 