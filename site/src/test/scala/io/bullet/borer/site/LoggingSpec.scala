package io.bullet.borer.site

import utest._

object LoggingSpec extends TestSuite {

  val tests = Tests {

    "Logging" - {
      def `only compiled, not actually run`() = {
        //#example
        import io.bullet.borer.Cbor

        val value = Map(
          "foo" -> Left(42),
          "bar" -> Right(Vector("For", "the", "King!"))
        )
        val encoded = Cbor.encode(value).toByteArray
        val decoded =
          Cbor
            .decode(encoded)
            .withPrintLogging() // just insert this line to enable logging
            .to[Map[String, Either[Int, Vector[String]]]]
            .value
        //#example
      }
    }
  }
}
