`borer-compat-scodec`
=====================

The @ref:[`borer-core`](borer-core/index.md) module by itself only knows how to encode to and decode from plain byte
arrays (`Array[Byte]`).

When you include the `borer-compat-scodec` module as a dependency (see the
@ref:[Getting Started](getting-started.md) chapter for details) and

```scala
import io.bullet.borer.compat.scodec._
```

you also get full "zero-copy" support for encoding to and decoding from `scodec.bits.ByteVector` as well as an implicit
`Encoder[ByteVector]` and `Decoder[ByteVector]`.