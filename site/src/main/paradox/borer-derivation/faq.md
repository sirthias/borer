Derivation FAQ
==============

Fully-automatic derivation (via the `deriveAll...` macros) comes with a few special cases/gotchas that are detailed on
this page.


### Custom Overrides

Fully-automatic derivation of encoders/decoders for ADTs allows for providing custom codecs for a subsets
of the type hierarchy. If a implicit typeclass for a certain sub-type is already available at the `deriveAll...` macro
call site then this implicit will be used rather than a new (and potentially conflicting) one generated.

Example:

@@snip [-]($test$/DerivationSpec.scala) { #example-adt }

@@snip [-]($test$/DerivationSpec.scala) { #custom-override }

If you provide a custom codec for a whole abstract branch of the ADT hierarchy then borer will offload encoding and
decoding of all sub-types of that branch to your custom codec.


### Explicit Upcasts

Sometimes the interplay of typeclasses and ADTs can become a bit confusing, especially when explicit upcasts are
required.

The important thing to remember is that _borer's_ typeclasses (`Encoder[T]`, `Decoder[T]` and `Codec[T]`) are
intentionally **invariant** in their type parameters. This means that they will only be found and used when the types
match up exactly.

With this ADT, for example:

@@snip [-]($test$/DerivationSpec.scala) { #example-adt }

the following codec definition gives you an `Encoder[Animal]` and a `Decoder[Animal]`,<br>nothing more, nothing less:

@@snip [-]($test$/DerivationSpec.scala) { #adt-fully-automatic-codec-derivation }

It does **not** give you an `Encoder[Dog]` or a `Decoder[Cat]`!<br>
If you want codecs for specific ADT sub-types like `Dog` or `Cat` you need to define them in addition to the codec for
the ADT super-type, e.g. like this:

@@snip [-]($test$/DerivationSpec.scala) { #adt-dog-cat-derivation }

An alternative would be to explicitly _upcast_ a value of a more specific type to the ADT super type, e.g. like this:

@@snip [-]($test$/DerivationSpec.scala) { #adt-default-encoding }

However, note that the encoding of a `Dog` as an `Animal` is not the same as the encoding of the `Dog` itself!!
The former includes the wrapping layer with the type-id while the latter doesn't:

@@snip [-]($test$/DerivationSpec.scala) { #animal-vs-dog }

This is because the `Decoder[Animal]` needs to somehow receive the information _which_ `Animal` type to decode into,
while the `Decoder[Dog]` doesn't, as it already knows the concrete target exactly.

So, while explicit upcasts are sometimes what you want there are also cases where they are not what you want.
The exact target types of your (de)serializations depends on your specific use case and, as such, should be chosen
carefully.


### Recursive ADTs

In most cases the codecs for non-generic ADTs are assigned to an `implicit val` so they don't have to be recreated on
every use. However, if the `ADT` is recursive the definition should be an `implicit lazy val` with an explicit type
annotation instead, as in this example:

@@snip [-]($test$/DerivationSpec.scala) { #recursive-adt }



        