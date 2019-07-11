package io.bullet.borer.site

import io.bullet.borer.internal.Util._
import utest._

object JsonSpecificsSpec extends TestSuite {

  val tests = Tests {

    "JSON specifics" - {

      "writeEmptyArray" - {
        //#writeEmptyArray
        import io.bullet.borer.Writer

        def writeEmptyArray(w: Writer): w.type =
          if (w.writingJson) w.writeArrayStart().writeBreak()
          else w.writeArrayHeader(0) // fixed-sized Arrays are not supported in JSON
        //#writeEmptyArray

        import io.bullet.borer._

        object Foo

        implicit val fooEncoder: Encoder[Foo.type] =
          Encoder((w, _) => writeEmptyArray(w))

        Cbor.encode(Foo).toByteArray ==> hex"80"
        Json.encode(Foo).toUtf8String ==> "[]"
      }

      "writeArrayOpen-close" - {
        //#writeArrayOpen-close
        import io.bullet.borer.Writer

        def writeAsUnaryArray(w: Writer, s: String): w.type =
          w.writeArrayOpen(1) // automatically chooses the most efficient
            .writeString(s)
            .writeArrayClose() // way to write an array of size one
        //#writeArrayOpen-close

        import io.bullet.borer._

        object Foo

        implicit val fooEncoder: Encoder[Foo.type] =
          Encoder((w, _) => writeAsUnaryArray(w, "foo"))

        Cbor.encode(Foo).toByteArray ==> hex"8163666F6F"
        Json.encode(Foo).toUtf8String ==> """["foo"]"""
      }
    }
  }
}
