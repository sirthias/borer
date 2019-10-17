Types Supported Out-of-the-Box
==============================

_borer_ comes with built-in encoding and decoding support for arbitrary combinations of the following types:

- `Boolean`, `Char`, `Byte`, `Short` `Int`, `Long`, `Float`, `Double` and their boxed counterparts
- `Null`
- `String`
- `Array[Byte]` 
- `java.math.BigInteger`, `java.math.BigDecimal` and their scala wrappers `BigInt` and `BigDecimal`
- `Option[T]` 
- `Array[T]` 
- `M[T] <: Iterable[T]` 
- `M[A, B] <: Map[A, B]`
- `Iterator[T]`  (encoding only!)
- `Either[A, B]` (with `import io.bullet.borer.Codec.ForEither.default`)
- `Tuple1[A]` ... `Tuple22[A, B, ... V]`

All these type are encoded to exactly one [CBOR] or [JSON] data item (which may of course be an array or map
consisting of other, nested data items.)

  [CBOR]: http://cbor.io/
  [JSON]: http://json.org/