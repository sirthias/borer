Array-Based Codecs
==================

Array-Based Codec derivation is enabled with this import:

@@snip [-]($test$/DerivationSpec.scala) { #import-array-based }

This brings the three methods `deriveEncoder[T]`, `deriveDecoder[T]` and `deriveCodec[T]` in scope,
which you can use to derive the respective type classes for case classes and ADTs.

Here is an example:

@@snip [-]($test$/DerivationSpec.scala) { #array-based }   

With these codecs case classes are written to [CBOR]/[JSON] as simple arrays, unless the case class has arity 1.
If the case class has only a single member, the derived codec directly writes the member without wrapping it in an
array.  

An _Abstract Data Type_ (ADT) is encoded as an array of length two, with the first element holding the type ID and
the second holding the instance's encoding (i.e. an array or single element).

Check out the chapter on @ref[Type IDs](type-ids.md) for more info on type IDs and how to customized them.

  [CBOR]: http://cbor.io/
  [JSON]: http://json.org/
  