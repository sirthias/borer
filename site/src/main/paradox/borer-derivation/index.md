`borer-derivation`
==================

When you include the `borer-derivation` module as a dependency (see the @ref:[Getting Started](../getting-started.md)
chapter for details) _borer_ can (semi-automatically) provide encoders and decoders for case classes and ADTs by
deriving them with the help of a macro (which is heavily inspired by [Magnolia]).

There are two basic alternatives to choose from:

- @ref:[Array-Based Codecs](array-based.md) or
- @ref:[Map-Based Codecs](map-based.md).

@@@index

* [-](array-based.md)
* [-](map-based.md)
* [-](type-ids.md)
* [-](semi-vs-full-automatic.md)

@@@

@@toc { depth=2 }

  [Magnolia]: https://propensive.com/opensource/magnolia