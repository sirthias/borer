<div style="display:flex">
  <img src="assets/images/borer-logo.svg" alt="borer Logo" style="flex:0 1 100px; margin: 0 4rem 3rem 0"/>
  <div style="flex:1 1 400px">
    <h1 style="font-size:10rem;font-weight:600;margin-bottom: 1rem;">borer</h1>
    Efficient <a href="http://cbor.io/">CBOR</a> and <a href="http://json.org/">JSON</a> (de)serialization for
    <a href="https://www.scala-lang.org/">Scala</a>.
  </div>
</div>

[![Maven Central](https://img.shields.io/maven-central/v/io.bullet/borer-core_2.12.svg)](https://maven-badges.herokuapp.com/maven-central/io.bullet/borer-core_2.12)
[![Uses Badges](https://img.shields.io/badge/uses-badges-c0ca33.svg)](http://shields.io/)

borer
=====

Complete
: _borer_ supports all [CBOR] and [JSON] features, incl. "over long" integers, 16-bit half-precision floats,
  `BigInteger` and `BigDecimal`, custom tags and "simple values".
  
Lightweight
: _borer_ has zero external dependencies and its code is relatively compact.

Fast
: _borer_ features DOM-less pull-parsers with one-element look-ahead and is designed for a good tradeoff between
  performance and usability. It supports stream parsing (in constant memory, even for very large files) and consistently
  @ref:[outperforms](borer-core/07-JSON-performance.md) most other serialization libraries targeting [CBOR] or
  [JSON] that are available to [Scala] developers.
 
Flexible
: _borer_ tries hard to make integration into your applications environment as seamless as possible.
  The type-class based design makes a lot of aspects deeply and easily customizable.<br>
  Also, since _borer_ offers a unified API for both, [CBOR] and [JSON], it's very easy to provide "bilingual" REST APIs
  that can consume and produce both formats interchangibly. 

Debuggable
: _borer_ offers convenient in-depth logging of the encoding/decoding process that is very helpful for debugging
  serialization issues, especially when working with a binary format like [CBOR].

Scala JS
: All _borer_ modules (except for `borer-compat-akka`) support [scala.js].


  [Scala]: https://www.scala-lang.org/
  [scala.js]: https://www.scala-js.org/
  [CBOR]: http://cbor.io/
  [JSON]: http://json.org/
  
  
@@@index

* [-](01-design-principles.md)
* [-](02-getting-started.md)
* [-](borer-core/index.md)
* [-](04-borer-derivation.md)
* [-](05-borer-compat-akka.md)
* [-](06-borer-compat-scodec.md)
* [-](project/index.md)

@@@