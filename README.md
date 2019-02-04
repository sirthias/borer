BORER
=====

a [CBOR] implementation in Scala sporting these features:

- complete (supports all [CBOR] features, incl. "over long" integers, 16-bit half-precision floats, `BigInteger` and
  `BigDecimal`, custom tags and "simple values")
- lightweight (`core` module has zero dependencies)
- fast (allocation-free in the core code paths, no DOM, pull-parser style)
- easy integration (type class-based design)
- supports custom "byte string" abstractions (like `akka.util.ByteString` or `scodec.bits.ByteVector`)
- no code generation, no macros (except for the `borer-derivation` module)

  [CBOR]: http://cbor.io/