`StringNumbers` and `StringBooleans`
====================================

Some external APIs, especially [JSON]-based ones, encode certain or all numbers, booleans and even `null` as string
values, even if the respective value _could_ be represented as a native [JSON] or [CBOR] number, boolean or `null`.

In order to make integration with these kind of services easy _borer_ comes with predefined encoders and decoders for
numbers, booleans and `null` that you can "enable" with simple imports and which then take precedence over the
default encoders and decoders.

This is how they are enabled:

@@snip [-]($test$/StringNumbersSpec.scala) { #imports }

As always these imports are "active" throughout the complete scope in which they are visible.

Here is an example demonstrating how to encode numbers and booleans as strings:

@@snip [-]($test$/StringNumbersSpec.scala) { #example }

Decoding works correspondingly.

  [CBOR]: http://cbor.io/
  [JSON]: http://json.org/