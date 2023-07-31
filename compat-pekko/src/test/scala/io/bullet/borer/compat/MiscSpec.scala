/*
 * Copyright (c) 2019-2023 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.compat

import _root_.org.apache.pekko.util.ByteString
import _root_.org.apache.pekko.actor.ActorSystem as ClassicActorSystem
import _root_.org.apache.pekko.actor
import _root_.org.apache.pekko.actor.typed.{ActorRef, ActorSystem}
import _root_.org.apache.pekko.actor.typed.scaladsl.Behaviors
import io.bullet.borer._
import io.bullet.borer.derivation.ArrayBasedCodecs

class MiscSpec extends AbstractBorerSpec {
  import pekko._

  def encode[T: Encoder](value: T): String   = toHexString(Cbor.encode(value).to[ByteString].result.toArray)
  def decode[T: Decoder](encoded: String): T = Cbor.decode(ByteString(hexBytes(encoded))).to[T].value

  case class Foo(int: Int, content: ByteString)

  implicit val fooCodec: Codec[Foo] = ArrayBasedCodecs.deriveCodec[Foo]

  test("roundtrip to and from ByteString") {
    roundTrip(
      "83820b40820c476f682079656168820d43ff0001",
      Vector(
        Foo(11, ByteString.empty),
        Foo(12, ByteString("oh yeah")),
        Foo(13, ByteString(-1, 0, 1))
      )
    )
  }

  test("roundtrip classic ActorRef") {

    class SomeActor extends actor.Actor {
      def receive = ???
    }

    implicit val system = actor.ActorSystem()
    val actorRef        = system.actorOf(actor.Props[SomeActor](), "some")

    decode[actor.ActorRef](encode(actorRef)) ==> actorRef
  }

  test("roundtrip typed ActorRef") {
    val system                                      = ActorSystem(Behaviors.ignore[Int], "some")
    val actorRef: ActorRef[Int]                     = system
    implicit def implicitSystem: ClassicActorSystem = system.classicSystem

    decode[ActorRef[Int]](encode(actorRef)) ==> actorRef
  }
}
