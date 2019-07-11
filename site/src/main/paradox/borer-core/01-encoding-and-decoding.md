Encoding and Decoding
=====================

Encoding a value to a plain `Array[Byte]`:

CBOR
: @@snip [-]($test$/EncodingDecodingSpec.scala) { #encoding-cbor }

JSON
: @@snip [-]($test$/EncodingDecodingSpec.scala) { #encoding-json }


Decoding a plain `Array[Byte]` back to a certain type:

CBOR
: @@snip [-]($test$/EncodingDecodingSpec.scala) { #decoding-cbor }

JSON
: @@snip [-]($test$/EncodingDecodingSpec.scala) { #decoding-json }

If you don't want _BORER_ to throw exceptions you can use the following variants to give you a `Try` instead:

CBOR
: @@snip [-]($test$/EncodingDecodingSpec.scala) { #try-cbor }

JSON
: @@snip [-]($test$/EncodingDecodingSpec.scala) { #try-json }

Or, if you prefer encoding/decoding to an `Either` instance:

CBOR
: @@snip [-]($test$/EncodingDecodingSpec.scala) { #either-cbor }

JSON
: @@snip [-]($test$/EncodingDecodingSpec.scala) { #either-json }


Additionally _borer_ offers even more top-level API calls that give you the respective `Output` or `Input` instances
either directly or wrapped in `Try` or `Either`. Check the sources of the central
@github[_borer_ API entry point](/core/src/main/scala/io/bullet/borer/Borer.scala) for more info.