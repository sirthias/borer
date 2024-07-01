`ByteStringArrayCodecs`
=======================

[CBOR] has a very compact encoding for byte arrays because binary data are “first-class”.<br>
While [JSON] itself has no such built-in support for binary representation [Base Encodings] can provide an often
acceptable work-around for the problem.
(See also @ref:[this chapter](JSON-specifics.md#base-encodings-for-binary-data) for more information about _borer_'s
out-of-the-box support for [Base Encodings]!)

Sometimes, however, the need arises to encode larger amounts of data as efficiently as binary data, even if they are
not immediately represented as byte arrays, for example arrays of primitive numbers such as:

- `Array[Short]`
- `Array[Int]`
- `Array[Long]`
- `Array[Float]`
- `Array[Double]`

By default _borer_ will encode these types like any other array, which can add quite a bit of overhead, especially
in the case of [JSON].

In order to allow for more efficient encodings (as well as faster encoding and decoding processes) _borer_
provides special codecs for these array types that you can "enable" with one of these two imports:

- `import io.bullet.borer.ByteStringArrayCodecs.BigEndian.given`
- `import io.bullet.borer.ByteStringArrayCodecs.LittleEndian.given`

With one of these imports in place an `Array[Int]` for example will be encoded as a raw byte array, with each `Int`
value being represented as four bytes (in the respective byte order). Conversion to and from byte arrays is done as
efficiently as the platform allows.

Also, the `ByteStringArrayCodecs` are compatible with custom byte array encoders (like the
@ref:[Base Encoders](JSON-specifics.md#base-encodings-for-binary-data)).

[CBOR]: http://cbor.io/

[JSON]: http://json.org/

[Base Encodings]: https://tools.ietf.org/html/rfc4648