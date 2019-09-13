`borer-compat-akka`
==================

`ByteString` support
--------------------

The @ref:[`borer-core`](borer-core/index.md) module by itself only knows how to encode to and decode from plain byte
arrays (`Array[Byte]`) and a few other types (like `java.nio.ByteBuffer`, see the chapter on
@ref:[Input, Output, ByteAccess](borer-core/06-supporting-typeclasses.md) for full details).

When you include the `borer-compat-akka` module as a dependency (see the
@ref:[Getting Started](02-getting-started.md) chapter for details) and

```scala
import io.bullet.borer.compat.akka._
```

you also get full "zero-copy" support for encoding to and decoding from `akka.util.ByteString` as well as an implicit
`Encoder[ByteString]` and `Decoder[ByteString]`.


Akka Http (Un)Marshalling
-------------------------

In addition to `ByteString` support the `borer-compat-akka` also provides convenient marshallers and unmarshallers
for HTTP entities.

When you include the `borer-compat-akka` module as a dependency (see the
@ref:[Getting Started](02-getting-started.md) chapter for details) and

```scala
import io.bullet.borer.compat.akkaHttp._
```
 
`akka-http` will transparently marshal to and from your custom domain model types:

@@snip [-]($test$/AkkaHttpSupportSpec.scala) { #example }
 
By default the `Unmarshaller` constructed by `borer-compat-akka` understand both [CBOR] and [JSON] with Content-Type
`application/cbor` and `application/json`, respectively.<br>
The `Marshaller` also supports both formats and lets the client determine via HTTP content negotiation (i.e. the
`Accept` header) , which one it prefers. If the client has no preference [CBOR] is chosen.

Also, (Un)marshaller construction can be customized in various ways, e.g. with custom media types.
Check out the @github[sources](/akka/src/main/scala/io/bullet/borer/compat/akkaHttp.scala) for full details.

  [CBOR]: http://cbor.io/
  [JSON]: http://json.org/ 