package nl.surfnet.nsiv2.persistence

import java.time.Instant
import java.util.UUID
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Shrink
import play.api.db.Databases
import play.api.test._
import nl.surfnet.nsiv2.messages.CorrelationId
import nl.surfnet.nsiv2.soap.Conversion

abstract class MessageStoreSpecification extends org.specs2.mutable.Specification with org.specs2.ScalaCheck {
  sequential

  private def newConnectionId = UUID.randomUUID.toString

  type Message
  implicit def MessageConversion: Conversion[Message, MessageData]
  implicit def MessageGenerator: Gen[Message]

  implicit def ArbitraryMessage = Arbitrary(MessageGenerator)
  implicit def ArbitraryMessageData = Arbitrary(for {
    message <- MessageGenerator
    data <- MessageConversion(message).map(Gen.const).getOrElse(Gen.fail)
  } yield (message, data))

  implicit def shrinkMessage: Shrink[Message] = Shrink(x => Stream.empty)

  class Fixture extends WithApplication() {
    lazy val database = Databases.inMemory()
    lazy val requesterNsa = "requester_msa"
    lazy val timestamp = Instant.now()
    lazy val aggregatedConnectionId = newConnectionId
    lazy val messageStore = {
      val s = new MessageStore[Message](database)
      s.create(aggregatedConnectionId, timestamp, requesterNsa)
      s
    }
  }

  "MessageStore" should {
    "fail to store message for unknown connection" in new Fixture() {
      prop { (message: Message) =>
        val unknownConnectionId = newConnectionId

        messageStore.storeInboundWithOutboundMessages(unknownConnectionId, timestamp, message, Seq.empty) must throwA[IllegalArgumentException]
      }.set(minTestsOk = 1)
    }

    "fail to store a message for a deleted connection" in new Fixture() {
      prop { (message: Message) =>
        messageStore.delete(aggregatedConnectionId, timestamp)

        messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, timestamp, message, Seq.empty) must throwA[IllegalArgumentException]
      }.set(minTestsOk = 1)
    }

    "append new messages at the end" in new Fixture {
      prop { (message: Message) =>
        messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, timestamp, message, Seq.empty) must not(throwA[Exception])

        val loaded = messageStore.findByConnectionId(aggregatedConnectionId)
        loaded.map(_.createdAt).lastOption must beSome(timestamp)
        loaded.map(_.message).lastOption aka "last appended message" must beSome(message)
      }.set(minTestsOk = 10)
    }

    "retrieve based on correlation id" in new Fixture {
      prop { (message: Message) =>
        withCorrelationId(message) { correlationId =>
          messageStore.storeInboundWithOutboundMessages(aggregatedConnectionId, timestamp, message, Seq.empty) must not(throwA[Exception])

          val loaded = messageStore.findByCorrelationId(requesterNsa, correlationId)
          loaded.map(_.message).lastOption aka "message" must beSome(message)
        }
      }.set(minTestsOk = 10)
    }
  }

  private def withCorrelationId(message: Message)(prop: CorrelationId => Prop): Prop = {
    MessageConversion(message).toOption.flatMap(_.correlationId).map(prop).getOrElse(Prop.undecided)
  }
}
