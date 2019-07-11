Basic Design Principles and Limitations 
=======================================

_borer_'s goal is to provide a highly efficient (de)serialization layer between some data model defined in [Scala] and
[CBOR] and/or [JSON] as storage/network formats. _borer_ performs this task in a very direct way, without relying on
some Abstract Syntax Tree (AST, also called Document Object Model (DOM)) as intermediate structure.

As such it doesn't offer any facilities for pre- or post-processing the serialized data, like manipulating the [JSON]
structure, filtering or augmenting nodes, or the like.

Also, it doesn't rely on reflection in any way. All information about the types to encode and decode must be statically
available at the encoding/decoding point. With the exception of `sealed` ADT hierarchies (which are supported out of
the box with the `borer-derivation` module) this means that you need to define yourself how to represent type
information of abstract types on the wire!

Another design principle has been to implement _borer_'s core module without relying on Scala macros or depending on any
external libraries. This should make _borer_ easily maintainable for the foreseeable future and reduces its weight as
a dependency of your applications (which can be especially important with [scala.js]).

(Note: The `borer-derivation` module _does_ rely on macros for deriving encoder/decoder type classes, but its use is
completely optional.

  [Scala]: https://www.scala-lang.org/
  [scala.js]: https://www.scala-js.org/
  [CBOR]: http://cbor.io/
  [JSON]: http://json.org/                        