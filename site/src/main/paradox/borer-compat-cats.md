`borer-compat-cats`
===================

The `borer-compat-cats` module provides default codecs for the following `cats.data._` data structures from the
[cats] `cats-core` package:

- `Chain[A]`
- `Ior[A, B]`
- `NonEmptyChain[T]`
- `NonEmptyList[T]`
- `NonEmptyMap[T]`
- `NonEmptySet[T]`
- `NonEmptyVector[T]`
- `Validated[A, B]`


Installation
------------
 
Include the `borer-compat-cats` module as a dependency (see the @ref:[Getting Started](getting-started.md) chapter for
details) and

```scala
import io.bullet.borer.compat.cats.*
```

With this import in place _borer_ will seamlessly encode and decode instances of the types listed above, provided that
encoders and/or decoders are available for the respective "inner" types.  


Encoding Strategies
-------------------

While the encoding strategies for most of the above data structures should be relatively straight-forward there are two
types, for which there is no clear default encoding:

- `Ior[A, B]`
- `Validated[A, B]`

The encodings implemented by `borer-compat-cats` aim for best time and space efficiency, not "readability".

One easy way to optimize for self-describability rather than efficiency would be to rely on `borer-derivation` instead.
Here is an example contrasting the difference:

@@snip [-]($test$/CatsCompatSpec.scala) { #example }

See the @github[module sources](/compat-cats/src/main/scala/io/bullet/borer/compat/cats.scala) for full details.

  [cats]: https://typelevel.org/cats/
