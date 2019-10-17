`Input`, `Output`, `ByteAccess`
===============================

In order to allow for seamless integration in all kinds of application environments _borer_ abstracts over decoding
input, encoding output as well as general "chunks of bytes" with three additional type classes `Input[T]`, `Output[T]`
and `ByteAccess[T]`.


Input
-----

For decoding _borer_ happily consumes any type for which an `Input.Provider[T]` is implicitly available,
which is responsible for constructing an @github[Input](/core/src/main/scala/io/bullet/borer/Input.scala)
from or around `T`:

@@snip [-]($core$/Input.scala) { #provider }

Currently _borer_ comes with predefined `Input` implementations for these types:

- `Array[Byte]`
- `java.nio.ByteBuffer`
- `java.io.InputStream`
- `java.io.File`
- `akka.util.ByteString` (with the `borer-compat-akka` module)
- `scodec.bits.ByteVector` (with the `borer-compat-scodec` module)
- `Iterator[T]`, provided that there is an `Input.Provider[T]` available implicitly

The latter is a great way to consume large input in a streaming fashion, without having to load everything into memory
at once. The `FromFileInput`, for example, relies on it to parse large files as an iteration of chunks.

The @github[Input](/core/src/main/scala/io/bullet/borer/Input.scala) trait isn't particularly hard to implement,
especially since it merely has to support single-pass access to the underlying bytes with minimal buffering and without
random access.


Output
------

On the encoding side _borer_ can either produce any type `T` for which an `Output.ToTypeProvider[T]` is available,
or "push" the output into a value of type `T` if an `Output.ToValueProvider[T]` is available:

@@snip [-]($core$/Output.scala) { #provider }

Currently _borer_ comes with predefined @github[Output](/core/src/main/scala/io/bullet/borer/Output.scala)
implementations for these types:

- `Array[Byte]`
- `java.nio.ByteBuffer`
- `java.io.OutputStream` (to a an existing instance)
- `java.io.File` (to a an existing instance)
- `akka.util.ByteString` (with the `borer-compat-akka` module)
- `scodec.bits.ByteVector` (with the `borer-compat-scodec` module)

The @github[Output](/core/src/main/scala/io/bullet/borer/Output.scala) trait isn't hard to implement as it simply writes
out all bytes in a single pass.

Here are a few examples to illustrate the top-level output API:

@@snip [-]($test$/OutputExamplesSpec.scala) { #examples }


ByteAccess
----------

Unfortunately Scala (and the whole JVM eco-system) has no single, versatile abstraction for a "chunk of bytes" that
fits the needs of all applications. In order to remain open to the preferences of the application _borer_ also
abstracts over "chunks of bytes" by allowing the use of any type `T`, for which a `ByteAccess[T]` is available.

Currently _borer_ comes with predefined @github[ByteAccess](/core/src/main/scala/io/bullet/borer/ByteAccess.scala)
implementations for these types:

- `Array[Byte]`
- `java.nio.ByteBuffer`
- `akka.util.ByteString` (with the `borer-compat-akka` module)
- `scodec.bits.ByteVector` (with the `borer-compat-scodec` module)