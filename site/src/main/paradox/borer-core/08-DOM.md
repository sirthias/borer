Document Object Model (DOM)
===========================

While _borer_'s core design is DOM-less, writing directly to and reading directly from the respective stream of
[CBOR]/[JSON] data items, it is sometimes convenient to nevertheless have access to an object structure that mirrors the
structure of a [CBOR]/[JSON] document as closely as possible.<br>
(Many [JSON]-libraries solely rely on such an intermediate "AST" structure for their encoding and decoding operations.)

For such cases _borer_ provides you with a simple "DOM" ADT (see the respective source file
@github[here](/core/src/main/scala/io/bullet/borer/Dom.scala)), which you can use like this:

CBOR
: @@snip [-]($test$/DomSpec.scala) { #cbor }

JSON
: @@snip [-]($test$/DomSpec.scala) { #json }


Any [CBOR] or [JSON] document can be deserialized into a `io.bullet.borer.Dom.Element`, which can then be queried or
transformed in any way, and potentially written back to [CBOR] or [JSON].<br>

As such, the _borer_ DOM can also be used to transform [JSON] to [CBOR], e.g. like this:

@@snip [-]($test$/DomSpec.scala) { #json-to-cbor }  

  [CBOR]: http://cbor.io/
  [JSON]: http://json.org/