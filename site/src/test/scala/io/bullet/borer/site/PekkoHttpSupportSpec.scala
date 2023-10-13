/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.site

import io.bullet.borer.BorerSuite

class PekkoHttpSupportSpec extends BorerSuite {

  test("example") {

    // #example
    import org.apache.pekko.http.scaladsl.server.Directives.*
    import org.apache.pekko.http.scaladsl.server.Route

    // borer encoders/decoders for the custom model above
    import io.bullet.borer.{Decoder, Encoder}
    import io.bullet.borer.derivation.MapBasedCodecs.*

    // custom model for request and response content
    final case class MyRequest(foo: String) derives Decoder
    final case class MyResponse(importantValue: Int) derives Encoder

    // bring automatic (un) marshaller construction in scope
    import io.bullet.borer.compat.pekkoHttp.*

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
    // #example
  }
}
