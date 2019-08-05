Type IDs
========

When encoding _Abstract Data Types_ (ADTs), i.e. super-types of `sealed` type hierarchies like `Animal` in this example:

@@snip [-]($test$/DerivationSpec.scala) { #example-adt }

the encoder needs to somehow transfer the information to the decoder, which of the defined sub-types to decode into.<br>
In the context of the example ADT above, when `Json.decode(encoding).to[Animal]` is called the decoder needs to know,
whether the encoding holds a `Dog`, a `Cat` or any of the other (well-defined) sub-types of `Animal`.

This is done through a "Type ID", which is a `String` or number that uniquely identifies each ADT sup-type.

By default _borer_ will use a sub-type's short class name as a (textual) type ID, i.e. `"Dog"` would be the type id of
`Dog` instances and `"Cat"` would be the same for the `Cat` type.

If you want to customize this you can use the `@key` annotation to do so.
Check out the `@key` sources @github[here](/derivation/src/main/scala/io/bullet/borer/derivation/key.scala) for more
info on this. 