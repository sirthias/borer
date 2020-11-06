package io.bullet.borer.site

import utest._

object AkkaHttpSupportSpec extends TestSuite {

  val tests = Tests {

    "example" - {

      //#example
      import akka.http.scaladsl.server.Route
      import akka.http.scaladsl.server.Directives._

      // custom model for request and response content
      final case class MyRequest(foo: String)
      final case class MyResponse(importantValue: Int)

      // borer encoders/decoders for the custom model above
      import io.bullet.borer.derivation.MapBasedCodecs._

      implicit val myRequestDecoder  = deriveDecoder[MyRequest]
      implicit val myResponseEncoder = deriveEncoder[MyResponse]

      // bring automatic (un) marshaller construction in scope
      import io.bullet.borer.compat.akkaHttp._

      // route that unmarshalls to `MyRequest` and marshals to `MyResponse`
      val route: Route =
        pathSingleSlash {
          post {
            entity(as[MyRequest]) { myRequest =>
              complete {
                MyResponse(myRequest.foo.length)
              }
            }
          }
        }
      //#example
    }
  }
}
