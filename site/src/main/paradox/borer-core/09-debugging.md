Debugging
========= 

With [CBOR] being a binary format debugging problems can sometimes be a bit more difficult than with text-based formats
like [JSON], which are immediately human-readable.

In order to help debug problems with encoding or decoding, independently of whether they are caused by your own
application code or invalid / unexpected input, you can easily switch on logging of the encoding or decoding process
with the `.withPrintLogging()` modifier.

For example, this snippet:

@@snip [-]($test$/LoggingSpec.scala) { #example } 

produces this logging output to the console:

```
1: {
    1/2: "foo"
    1/2: -> {
        1/1: 0
        1/1: -> 42
    1/2: }
    2/2: "bar"
    2/2: -> {
        1/1: 1
        1/1: -> [
            1/3: "For"
            2/3: "the"
            3/3: "King!"
        1/1: ]
    2/2: }
1: }
2: END
```

which makes it very easy to see, what exactly the decoding input consists of.<br>
Also, in case of any exception the log will break off exactly at the point where the exception got thrown,
which is often a great help in locating the problem.

  [CBOR]: http://cbor.io/
  [JSON]: http://json.org/