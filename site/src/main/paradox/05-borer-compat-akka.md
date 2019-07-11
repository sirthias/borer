`borer-compat-akka`
==================

The @ref:[`borer-core`](borer-core/index.md) module by itself only knows how to encode to and decode from plain byte
arrays (`Array[Byte]`).

When you include the `borer-compat-akka` module as a dependency (see the
@ref:[Getting Started](02-getting-started.md) chapter for details) and

```scala
import io.bullet.borer.compat.akka._
```

you also get full "zero-copy" support for encoding to and decoding from `akka.util.ByteString` as well as an implicit
`Encoder[ByteString]` and `Decoder[ByteString]`.