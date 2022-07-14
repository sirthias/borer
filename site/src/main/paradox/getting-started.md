Getting Started
===============
 
Modules
-------

_borer_ consists of these modules:

- @ref:[`borer-core`](borer-core/index.md), the actual core logic (no dependencies)
- @ref:[`borer-derivation`](borer-derivation/index.md), (semi-)automatic codec derivation for case classes and ADTs<br>(no dependencies, relies on macros)
- @ref:[`borer-compat-akka`](borer-compat-akka.md), support for `akka.util.ByteString` and `akka-http` (un)marshalling<br>(has a `provided` dependency on [akka-actor], [akka-stream] and [akka-http])
- @ref:[`borer-compat-cats`](borer-compat-cats.md), support for popular [cats] data structures from the `cats.data._` package (has a `provided` dependency on `cats-core`)
- @ref:[`borer-compat-circe`](borer-compat-circe.md), seamless integration with [circe] codecs<br>(has a `provided` dependency on [circe])
- @ref:[`borer-compat-scodec`](borer-compat-scodec.md), support for `scodec.bits.ByteVector`<br>(has a `provided` dependency on [scodec])

Installation
------------

The artifacts for _borer_ live on Maven Central and can be tied into your project like this:


@@dependency[sbt,Maven,Gradle] {
  group="io.bullet" artifact="borer-core_3" version="$project.version$"
  group2="io.bullet" artifact2="borer-derivation_3" version2="$project.version$"
  group3="io.bullet" artifact3="borer-compat-akka_3" version3="$project.version$"
  group3="io.bullet" artifact3="borer-compat-cats_3" version4="$project.version$"
  group4="io.bullet" artifact4="borer-compat-circe_3" version5="$project.version$"
  group5="io.bullet" artifact5="borer-compat-scodec_3" version6="$project.version$"
}

_borer_ is available for [Scala] 3.x, both for the JVM as well as [scala.js].

There is also a version for Scala 2.13, whose latest version number is 1.8.0.  
Currently there is still feature parity between the Scala 3 and Scala 2.13 versions, but future development will
focus on the newer Scala 3 code only.

  [Scala]: https://www.scala-lang.org/
  [scala.js]: https://www.scala-js.org/
  [akka-actor]: https://doc.akka.io/docs/akka/2.5/actors.html#dependency
  [akka-stream]: https://doc.akka.io/docs/akka/current/stream/index.html
  [akka-http]: https://doc.akka.io/docs/akka-http/current/index.html
  [cats]: https://typelevel.org/cats/
  [circe]: https://circe.github.io/circe/
  [scodec]: http://scodec.org/