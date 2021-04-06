Transcoding
===========
           
Apart from @ref:[Encoding and Decoding](encoding-and-decoding.md) _borer_ also supports a less frequently used operation
called "transcoding", which combines encoding from a type `A` and immediately decoding to a type `B` into one single
operation.

The point of transcoding isn't to generate some byte stream serialization but rather to make use of the existing (and
often carefully managed) codec logic to transform `A` into some other representation, which is easier to inspect or
process further.

For example, one could transform values of `A` into _borer_'s @ref:[DOM](DOM.md) representation and walk the resulting
object tree to collect all members called `foo`. Or apply some kind of tree transformation before transcoding the
@ref:[DOM](DOM.md) instance back to `A`.


Transcoding vs. Encoding + Decoding
-----------------------------------
                              
Since _transcoding_ is essentially encoding from one type before immediately decoding to another the same result can
be achieved by doing exactly that: two completely separate steps connected only by the intermediate, serialized
representation of the data in a byte array for example.

The advantage of _borer_'s dedicated transcoding operation over separate Encoding + Decoding is the much better
performance and decreased overhead. Transcoding connects the respective `Encoder[A]` and `Decoder[B]` stages as directly
as possible. Instances of basic types like `Int`, `Long`, `Double` and, especially, `String` don't need to be serialized
and then parsed and recreated, but can be directly reused, which greatly helps with keeping time and memory requirements
to a minimum.

Due to _borer_'s pull-based design transcoding still requires some intermediate buffering but this happens not on the
level of serialized bytes but on a higher level and as such allows the transcoding infrastructure to itself run with
practically no allocation overhead (when buffer caching isn't explicitly disabled).
Of course the `Decoder[B]` will usually still need to allocate new memory for the (sub-)structures of `B` instances. 


Transcoding as AST-Substitute
-----------------------------
                                                                               
Even though AST-less by design, support for efficient transcoding allows _borer_ to be used for tasks that typically
require an AST representation, like programmatic inspection/transformation of runtime object structures.
Transcoding to something like _borer_'s @ref:[DOM](DOM.md) representation can never fail and, because the
@ref:[DOM](DOM.md) implementation shipped with _borer_ is not special in any way, you are free to change it or write
your own from scratch without too much effort should the need arise.


CBOR vs. JSON Transcoding
-------------------------

Since transcoding skips the actual rendering to and parsing from raw bytes it is, in principle, agnostic to the question
of whether [CBOR] or [JSON] is the encoding target. Nevertheless, the `Encoder` and `Decoder` implementations also have
access to the information what the encoding target is and frequently use that information to adapt their logic
accordingly.
As such, you'll have to decide on whether to target [CBOR] or [JSON] even for transcoding.


Example
-------

@@snip [-]($test$/TranscodingSpec.scala) { #example }

  [CBOR]: http://cbor.io/
  [JSON]: http://json.org/