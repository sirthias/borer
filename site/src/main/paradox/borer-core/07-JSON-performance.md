JSON Performance
================

_borer_ comes with a fast JSON parser optimized for modern superscalar 64-bit CPUs, which performs quite well when
compared to other popular JSON libraries in the scala eco-system when it comes to transforming raw JSON-bytes  into a
case-class based data model.

A @github[benchmark against 18 real-world JSON data examples](/benchmarks/src/main/resources/) from a diverse set of
sources shows that _borer_'s JSON parsing infrastructure outperforms [Circe] (for example) by almost a factor of two,
on average:

@@@ div { #benchmark-results }

![Benchmark Results](.../benchmark-results.svg)

@@@

This is the benchmark setup:

Task
: Decode 18 files from raw [JSON] bytes into a respective
  @github[case-class based data model](/benchmarks/src/main/scala/io/bullet/borer/benchmarks/Model.scala)
  generated with https://transform.now.sh/json-to-scala-case-class/

JMH Parameters
: `-wi10 -i10 -t4 -f4`  

JVM
: Java HotSpot(TM) 64-Bit GraalVM EE 19.1.0

Memory Settings
: `-Xms4096m -Xmx4096m`

Scala Version
: 2.12.8

Operating System
: Ubuntu 18.04.2 LTS

CPU
: 2 x Intel Xeon E5-2665 2.4 - 3.1 GHz (Octa-core)

The `benchmarks` SBT sub project contains everything you need to run the benchmarks yourself.
This SBT command will produce a JSON file holding all results:

    jmh:run io.bullet.borer.benchmarks.* -wi 10 -i 10 -t 4 -f 4 -foe true -rf json -rff results/my_results.json  

@@@ note

This "full" benchmark will run for more than 4 days!

@@@

Since interpreting the results can be a bit tedious _borer_ comes with some simple analysis logic that can be invoked
with the `benchmarkResults` SBT command. It will show you a synthesis of all benchmark results currently living in the
`benchmarks/results` folder.

@@@ warning

The `benchmarks/results` folder already contains the results of the last "official" benchmarking runs.<br>
When you run your own benchmarks you should first remove these "official" `.json` files since benchmark results are
not comparable when run on non-identical machines!

@@@ 
 
  [JSON]: http://json.org/
  [Circe]: https://circe.github.io/circe/