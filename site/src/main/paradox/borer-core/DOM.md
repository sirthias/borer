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
transformed in any way, and potentially written back to [CBOR] or [JSON].
Make sure to also look at _borer_'s built-in support for efficient @ref:[transcoding](transcoding.md) if that is
something you are interested in.<br>

As such, the _borer_ DOM can also be used to transform [JSON] to [CBOR], e.g. like this:

@@snip [-]($test$/DomSpec.scala) { #json-to-cbor }


DOM Transformation
------------------

Having a DOM representation of a particular data structure (e.g. nested case class instances representing application
state) opens up a variety of opportunities for runtime inspection and/or transformation of that data structure,
which are often not that easy to achieve otherwise.

One solution to such tasks can be "optics" (like Lenses) for which dedicated libraries like [Monocle] exist.
They are an excellent tool if the point of interest/change is deep and relatively small compared to the total size of
the overage data structure.

However, if many changes need to be done or the data structure must be walked in its completion (for example in order to
gather some statistics) transcoding into a DOM and working on that alternate representation could be easier.

_borer_ provides one relatively simple tool for transforming an existing DOM structure:<br>
the @github[DOM.Transformer](/core/src/main/scala/io/bullet/borer/Dom.scala#L444) trait. 

See the chapter on @ref:[transcoding](transcoding.md) for an example of how to use it.


  [CBOR]: http://cbor.io/
  [JSON]: http://json.org/
  [Monocle]: https://www.optics.dev/Monocle/