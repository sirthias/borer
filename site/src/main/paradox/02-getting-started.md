Getting Started
===============
 
Modules
-------

_borer_ consists of these modules:

- @ref:[`borer-core`](borer-core/index.md), the actual core logic (no dependencies)
- @ref:[`borer-derivation`](04-borer-derivation.md), (semi-)automatic codec derivation for case classes and ADTs (no dependencies, relies on macros)
- @ref:[`borer-compat-akka`](05-borer-compat-akka.md), support for `akka.util.ByteString` (depends on [akka-actor])
- @ref:[`borer-compat-scodec`](06-borer-compat-scodec.md), support for `scodec.bits.ByteVector` (depends on [scodec])

Installation
------------

The artifacts for _borer_ live on Maven Central and can be tied into your project like this:

@@dependency[sbt,Maven,Gradle] {
  group="io.bullet" artifact="borer-core" version="$project.version$"
  group2="io.bullet" artifact2="borer-derivation" version2="$project.version$"
  group3="io.bullet" artifact3="borer-compat-akka" version3="$project.version$"
  group4="io.bullet" artifact4="borer-compat-scodec" version4="$project.version$"
}

_borer_ is available for [Scala] 2.12 and 2.13 as well as [scala.js].

  [Scala]: https://www.scala-lang.org/
  [scala.js]: https://www.scala-js.org/
  [akka-actor]: https://doc.akka.io/docs/akka/2.5/actors.html#dependency
  [scodec]: http://scodec.org/