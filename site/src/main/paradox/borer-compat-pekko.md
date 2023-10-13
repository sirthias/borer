`borer-compat-pekko`
====================

`ByteString` support
--------------------

The @ref:[`borer-core`](borer-core/index.md) module by itself only knows how to encode to and decode from plain byte
arrays (`Array[Byte]`) and a few other types (like `java.nio.ByteBuffer`, see the chapter on
@ref:[Input, Output, ByteAccess](borer-core/supporting-typeclasses.md) for full details).

When you include the `borer-compat-pekko` module as a dependency (see the
@ref:[Getting Started](getting-started.md) chapter for details) and

```scala
import io.bullet.borer.compat.pekko.*
```

you also get:

- full "zero-copy" support for encoding to and decoding from `pekko.util.ByteString`
- implicit codecs for `pekko.util.ByteString`, `pekko.actor.ActorRef` and `pekko.actor.typed.ActorRef[T]`


Pekko Http (Un)Marshalling
--------------------------

In addition to `ByteString` support the `borer-compat-pekko` also provides convenient marshallers and unmarshallers
for HTTP entities.

When you include the `borer-compat-pekko` module as a dependency (see the
@ref:[Getting Started](getting-started.md) chapter for details) and

```scala
import io.bullet.borer.compat.pekkoHttp.*
```

`pekko-http` will transparently marshal to and from your custom domain model types:

@@snip [-]($test$/PekkoHttpSupportSpec.scala) { #example }

By default the `Unmarshaller` constructed by `borer-compat-pekko` understand both [CBOR] and [JSON] with Content-Type
`application/cbor` and `application/json`, respectively.<br>
The `Marshaller` also supports both formats and lets the client determine via HTTP content negotiation (i.e. the
`Accept` header) , which one it prefers. If the client has no clear preference, i.e. it accepts both [CBOR] and [JSON]
with equal "q-value" (preference weight) the `Marshaller` choses [JSON] by default. This default, however, can be
configured to [CBOR] if needed.

(Un)marshaller construction can be customized in various ways, e.g. with custom media types, and is also available
for streams, i.e. to and from `pekko.stream.scaladsl.Source[T, _]` rather than simply `T`.<br>
Check out the @github[sources](/compat-pekko/src/main/scala/io/bullet/borer/compat/pekkoHttp.scala) for full details.

[CBOR]: http://cbor.io/
[JSON]: http://json.org/ 
