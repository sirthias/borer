JSON Specifics
==============

Since the [CBOR] data item primitives are a super-set of what is available in [JSON], or, said differently, everything
in [JSON] has a counterpart in [CBOR], it's not hard for _borer_ to also support encoding to and decoding from [JSON].

From _borer_'s point of view [JSON] is simply a slightly different binary format that only supports a subset of the
[CBOR] data primitives. Like [CBOR] _borer_ encodes and decodes [JSON] in a *single pass*, UTF-8 encoding and
decoding to and from raw bytes on the fly.

All higher-level abstractions (i.e. `Writer`, `Reader`, `Encoder`, `Decoder`, `Codec`, etc.) are essentially agnostic
to the (de)serialization target format. Nevertheless, the `Writer` and `Reader` types do have a `target` member, which
enables custom logic to discriminate between the two variants, if required.<br>
Since the underlying [JSON] renderer will throw exceptions on attempts to write data primitives that are not supported
in [JSON] (like [CBOR] Tags, for example), this is sometimes necessary to efficiently support both formats.

For example, in order to write an empty array in the most efficient way to both [CBOR] and [JSON] using only the most
low-level data items one would use this approach:

@@snip [-]($test$/JsonSpecificsSpec.scala) { #writeEmptyArray }

But on a slightly higher level the `Writer` also gives you a way to write arrays (and maps) without having distinguish
between [CBOR] and [JSON] yourself:

@@snip [-]($test$/JsonSpecificsSpec.scala) { #writeArrayOpen-close } 

As long as you rely on the somewhat higher-level parts of the `Reader` and `Writer` APIs or construct your
(de)serialization purely logic from _borer_'s built-in or (and/or @ref:[derived](../borer-derivation/index.md)) Encoders and
Decoders, your application will support both [CBOR] and [JSON] at the same time without any special casing whatsoever.


Base Encodings for Binary Data
------------------------------

One big drawback of [JSON] over [CBOR] is that [JSON] doesn't provide any "first-class" representation of binary data.
This is typically worked around by mapping binary data to a "Base Encoding", e.g. the ones defined by [RFC 4648].

In order to give your application an easy and flexible way to integrate with other systems _borer_ supports a number
of base encodings out of the box, specifically:

- [base16](https://tools.ietf.org/html/rfc4648#section-8)  
- [base32](https://tools.ietf.org/html/rfc4648#section-6)
- [base32hex](https://tools.ietf.org/html/rfc4648#section-7)
- [base32crockford](https://en.wikipedia.org/wiki/Base32#Crockford's_Base32)
- [z-base32](http://philzimmermann.com/docs/human-oriented-base-32-encoding.txt)
- [base64]
- [base64url](https://tools.ietf.org/html/rfc4648#section-5)

The default [JSON] encoding for `Array[Byte]` is [base64].

In order to switch to a different base encoding in a particular scope define the a pair of implicits as in this example:

@@snip [-]($test$/JsonSpecificsSpec.scala) { #alternative-base-encoding }


When (not) to use _borer_ for JSON
----------------------------------

Since _borer_ treats [JSON] as a binary format and reads/writes from/to raw bytes it isn't optimized for consuming or
producing Strings as input or output. (Strings have to first be UTF-8 encoded in order to be readable by _borer_.)<br>
So, if you need to frequently consume `String` input other [JSON] libraries will likely perform better.
Also, if you need to manipulate the [JSON] structures in any way between (de)serializing from/to the wire and from/to
your data model then _borer_ will not help you and a DOM/AST-based [JSON] solution (like [Circe]) will likely be the
better choice.

However, if all you need is an efficient way to convert raw network- or disk-bytes holding UTF-8 encoded [JSON] to and
from your data model types, with no (or few) dependencies and maybe even with the option to target [CBOR] with no
additional work required from your side, then _borer_ should be a good choice.

@@@ note

Since _borer_ doesn't really work with `String` and `Char`, but rather raw bytes only, it also doesn't support
encoding to a "pretty", i.e. nicely formatted, JSON representation.<br>
_borer_ always outputs [JSON] in the most compact form.
(It can, of course, read "pretty" [JSON] documents without any issue.)  

@@@


Comparison with other Scala JSON Libraries
------------------------------------------

(Additions, corrections, improvement suggestions very welcome, especially to this section!)

---

[Circe]
: AST/DOM- and type-class-based design

@@@ div { .pros }
 
- very mature
- allows for extensive DOM-manipulation
- many integration option already available
- compatible with [scala.js]

@@@

@@@ div { .cons }

- depends on `cats-core`
- type class derivation can be slow (at compile time)
- _borer_ is about twice as fast
- no [CBOR] support

@@@

---
    
[spray-json]
: AST/DOM- and type-class-based design

@@@ div { .pros }
 
- zero dependencies

@@@

@@@ div { .cons }

- pre-historic, clunky type class design
- essentially unmaintained
- no direct support for case classes w/ more than 22 members
- no type class derivation for ADTs
- _borer_ is about 2.5 times as fast
- not compatible with [scala.js]    
- no [CBOR] support

@@@

---
    
[µPickle]
: pull-style, type class-based design

@@@ div { .pros }

- zero dependencies
- optional DOM 
- also supports [MessagePack]
- compatible with [scala.js]

@@@

@@@ div { .cons }

- no support for case classes w/ more than 64 members
- no support for manual (no-macro) codec construction
- _borer_ is about 3 times as fast
- no [CBOR] support
    
@@@

---
    
[Jackson Scala]
: Java implementation with a Scala add-on

@@@ div { .pros }

- very mature
- good performance

@@@

@@@ div { .cons }
     
- no type class-based API
- several non-Scala dependencies
- not compatible with [scala.js]

@@@

---
        
[Jsoniter Scala]
: pull-style, type class-based design

@@@ div { .pros }

- zero dependencies
- very high performance (about 30% faster than _borer_)
- highly configurable

@@@

@@@ div { .cons }

- very low-level API
- entirely macro-based
- no pre-defined AST/DOM
- not compatible with [scala.js]
- no [CBOR] support

@@@

---

  [CBOR]: http://cbor.io/
  [JSON]: http://json.org/
  [RFC 4648]: https://tools.ietf.org/html/rfc4648
  [base64]: https://tools.ietf.org/html/rfc4648#section-4
  [Circe]: https://circe.github.io/circe/
  [spray-json]: https://github.com/spray/spray-json/
  [json-benchmark-files]: https://github.com/sirthias/borer/tree/master/benchmarks/src/main/resources
  [Jackson Scala]: https://github.com/FasterXML/jackson-module-scala
  [µPickle]: http://www.lihaoyi.com/upickle/
  [Jsoniter Scala]: https://github.com/plokhotnyuk/jsoniter-scala
  [MessagePack]: https://msgpack.org/
  [scala.js]: https://www.scala-js.org/