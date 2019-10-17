Map-Based Codecs
================

Map-based codec derivation is enabled with this import:

@@snip [-]($test$/DerivationSpec.scala) { #import-map-based }

With these codecs case classes are encoded as [CBOR]/[JSON] maps with the member name as key, i.e. the popular
"standard" way for serializing to and from [JSON]. 

Here is an example relying on map-based codecs:

@@snip [-]($test$/DerivationSpec.scala) { #map-based }


### ADT Encoding

Like @ref:[Array-Based Codecs](array-based.md) the Map-based codecs come with built-in support for Abstract Data
Types (ADT), e.g.

@@snip [-]($test$/DerivationSpec.scala) { #example-adt }

Since, up to now, _borer_ doesn't support fully-automatic derivation (but only semi-automatic, see also
@ref:[Semi- vs. Fully-automatic](semi-vs-full-automatic.md)) the current best practice for deriving codecs for ADTs
looks like this:

@@snip [-]($test$/DerivationSpec.scala) { #adt-codec-derivation }

Each ADT sub-type requires its own implicit codec (or encoder/decoder) definition with a call to the `deriveCodec` (or
`deriveEncoder` / `deriveDecoder`) macro. If the sub-type codecs are not required for anything else but the ADT codec
derivation the best practice is to move them into a sub-block scope as shown above.

The default encoding for ADT super-types is a single-entry map, with the key being the type id and the value
becoming the encoding of the actual ADT subtype instance, e.g. like this ([JSON] example):

```
{ "Dog" :
  {
    "age":2,
    "name":"Rex"
  }
}
```

This is how you get to this encoding (Note the explicit upcast to the ADT super-type `Animal`!):

@@snip [-]($test$/DerivationSpec.scala) { #adt-default-encoding }


#### Alternative "Flat" Encoding of ADTs

The default encoding for ADTs (single-entry maps) cleanly separates the encoding of the ADT super-type from the encoding
of the ADT sub-types by introducing a dedicated "envelope" layer holding the type ID in an efficient form. This encoding
can be written, stored and read in a very efficient manner and is therefore recommended for all application where you
have full control over the both ends of the encoding "channel".

However, especially when interoperating with other systems it is sometimes required to support another encoding style,
which carries the type ID in a special map entry, e.g. like this:

```
{
  "_type": "Dog",
  "age":2,
  "name":"Rex"
}
```  

This alternative ADT encoding can be switched to by making the result of a call to `AdtEncodingStrategy.flat()`
implicitly available:

@@snip [-]($test$/DerivationSpec.scala) { #flat-adt-encoding }

While this "flat" ADT encoding has the benefit that the encoding can be decoded into the ADT super-type _or_ the
respective ADT sub-type (provided that the decoder simply ignores surplus members, as _borer_ does) it also has a number
of significant drawbacks:

- It requires all ADT sub-types to be encoded to maps, i.e. it violates separation of concerns between the super- and
  sub-types. This can be annoying, e.g. when unary case classes could otherwise use `ArrayBasedCodecs.deriveUnaryCodec`
  (which encodes to the unwrapped parameter value) for more efficiency.

- It conflates two conceptually well separated and distinct name spaces: The one of the envelope (ADT super-type) and
  the one of the sub-type. This always leads to the risk of name collisions, which gives rise to type member names like
  `$type` or `__type` or the like, which make the hacky "smell" of the approach even more visible.
  
- Due the unordered nature of JSON objects/maps it makes efficient single-pass parsing impossible in the general case,
  since the information, how to interpret an object's members (the type of the object) might be delayed until the very
  end of the input file.<br>
  This requires either potentially unbounded caching or a second pass over the input. _borer's_ approach is the former
  (caching) with a configurable bound on the cache size (triggering an exception, when exceeded).

- It's slightly less efficient storage-wise (depending on the length of the `typeMemberName`).

For these reasons _borer's_ default ADT encoding relying on single-element maps is generally the preferred choice.


### Default Values

Map-based codecs support missing and extra members.

During encoding a member whose value matches the default value is not written to the output at all.
During decoding the type's `Decoder` will use the potentially defined default value for all missing members, e.g.:

@@snip [-]($test$/DerivationSpec.scala) { #default-value } 

Extra members (i.e. map keys present in the encoding but not defined as a case class member or `@key`) are simply ignored.<br>

Also, `Encoder`/`Decoder` type classes can implement the `Encoder.DefaultValueAware` / `Decoder.DefaultValueAware`
trait in order to alter their behavior in the presence of a default value.

This is used, for example, by the pre-defined encoder and decoder for `Option[T]`, which change their encoding/decoding
strategy, if a `None` default value is defined for a case class member. In this case the optional value will only be
written if it's defined (and then without any wrapping structure). If the option is undefined nothing is written at all.

Here is the actual implementation of _borer_'s `Encoder` for `Option`:

@@snip [-]($core$/Encoder.scala) { #option-encoder }


Correspondingly, during decoding the presence of the member yields a defined option instance holding the decoded value
and `None`, if the member is missing.

This behavior should match the intution of what an `Option[T]` case class member would behave like when written to a
[JSON] representation. 


### Customized Member Keys

_borer_ supports customizing the name of case class members in the encoding with the same `@key` annotation, that is
also used for custom ADT type-ids (see @ref:[Array-Based Codec](array-based.md) and the `@key` sources
@github[here](/derivation/src/main/scala/io/bullet/borer/derivation/key.scala) for more info).

Simply annotate a case class member do provide a custom name:

@@snip [-]($test$/DerivationSpec.scala) { #custom-member-name }


  [CBOR]: http://cbor.io/
  [JSON]: http://json.org/