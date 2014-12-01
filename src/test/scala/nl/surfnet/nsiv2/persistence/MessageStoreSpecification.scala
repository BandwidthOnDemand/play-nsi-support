package nl.surfnet.nsiv2.persistence

import java.net.URI
import java.time.Instant
import java.util.UUID
import nl.surfnet.nsiv2.soap.Conversion
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.specs2.execute.AsResult
import play.api.db.DB
import play.api.test._
import scala.util.Try

abstract class MessageStoreSpecification extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  sequential

  private def newConnectionId = UUID.randomUUID.toString

  type Message
  implicit def MessageConversion: Conversion[Message, MessageData]
  implicit def MessageGenerator: Gen[Message]

  implicit def ArbitraryMessage = Arbitrary(MessageGenerator)

  class Fixture extends WithApplication() {
    lazy val timestamp = Instant.now()
    lazy val aggregatedConnectionId = newConnectionId
    lazy val messageStore = {
      val s = new MessageStore[Message]("default")
      s.create(aggregatedConnectionId, timestamp, "requester_nsa")
      s
    }
  }

  "MessageStore" should {
    "fail to store message for unknown connection" in new Fixture() {
      propNoShrink { (message: Message) =>
        val unknownConnectionId = newConnectionId

        messageStore.storeInboundWithOutboundMessages(unknownConnectionId, timestamp, message, Seq.empty) must throwA[IllegalArgumentException]
      }.set(minTestsOk = 1)
    }

    "fail to store a message for a deleted connection" in new Fixture() {
      propNoShrink { (message: Message) =>
        messageStore.delete(aggregatedConnectionId, timestamp)

        messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, timestamp, message, Seq.empty) must throwA[IllegalArgumentException]
      }.set(minTestsOk = 1)
    }

    "append new messages at the end" in new Fixture {
      propNoShrink { (message: Message) =>
        messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, timestamp, message, Seq.empty) must not(throwA[Exception])

        val loaded = messageStore.loadAll(aggregatedConnectionId)
        loaded.map(_.createdAt).lastOption must beSome(timestamp)
        loaded.map(_.message).lastOption aka "last appended message" must beSome(message)
      }.set(minTestsOk = 10)
    }
  }
}
