<div style="display:flex">
  <img src="assets/images/borer-logo.svg" alt="borer Logo" style="width: 100px; margin: 0 4rem 3rem 0"/>
  <div style="flex:1 1 400px">
    <h1 style="font-size:10rem;font-weight:600;margin-bottom: 1rem;">borer</h1>
    Efficient <a href="http://cbor.io/">CBOR</a> and <a href="http://json.org/">JSON</a> (de)serialization for
    <a href="https://www.scala-lang.org/">Scala</a>.
  </div>
</div>

[![Maven Central](https://img.shields.io/maven-central/v/io.bullet/borer-core_3.svg)](https://maven-badges.herokuapp.com/maven-central/io.bullet/borer-core_3)
[![Build Status](https://travis-ci.org/sirthias/borer.svg?branch=master)](https://travis-ci.org/sirthias/borer)
[![codecov](https://codecov.io/gh/sirthias/borer/branch/master/graph/badge.svg)](https://codecov.io/gh/sirthias/borer)
[![Scala.js](https://www.scala-js.org/assets/badges/scalajs-0.6.17.svg)](https://www.scala-js.org)
[![Scala Steward](https://img.shields.io/badge/Scala_Steward-helping-brightgreen.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://scala-steward.org)
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
  @ref:[outperforms](borer-core/JSON-performance.md) most other serialization libraries targeting [CBOR] or
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
: All _borer_ modules (except for `borer-compat-akka` and `borer-compat-pekko`) support [scala.js].


  [Scala]: https://www.scala-lang.org/
  [scala.js]: https://www.scala-js.org/
  [CBOR]: http://cbor.io/
  [JSON]: http://json.org/
  
  
@@@index

* [-](design-principles.md)
* [-](getting-started.md)
* [-](borer-core/index.md)
* [-](borer-derivation/index.md)
* [-](borer-compat-akka.md)
* [-](borer-compat-pekko.md)
* [-](borer-compat-cats.md)
* [-](borer-compat-circe.md)
* [-](borer-compat-scodec.md)
* [-](project/index.md)

@@@