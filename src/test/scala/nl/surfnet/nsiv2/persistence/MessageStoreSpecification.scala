package nl.surfnet.nsiv2.persistence

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.UUID
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Shrink
import nl.surfnet.nsiv2.messages.CorrelationId
import nl.surfnet.nsiv2.soap.Conversion
import play.api.db.Database

abstract class MessageStoreSpecification
    extends org.specs2.mutable.Specification
    with org.specs2.ScalaCheck:
  sequential

  private def newConnectionId = UUID.randomUUID.toString

  type Message
  given MessageConversion: Conversion[Message, MessageData]
  given MessageGenerator: Gen[Message]

  /** A database with the `connections` and `messages` tables. */
  def database: Database

  given Arbitrary[Message] = Arbitrary(MessageGenerator)
  given Shrink[Message] = Shrink(_ => Stream.empty)

  private lazy val messageStore = new MessageStore[Message](database)

  class Fixture:
    val requesterNsa = "requester_nsa"
    // The store keeps milliseconds.
    val timestamp: Instant = Instant.now().truncatedTo(ChronoUnit.MILLIS)
    val connectionId = newConnectionId
    messageStore.create(connectionId, timestamp, requesterNsa)

    def store(inbound: Message, outbound: Message*): Unit =
      messageStore.storeInboundWithOutboundMessages(connectionId, timestamp, inbound, outbound)
    def messages: Seq[Message] = messageStore.findByConnectionId(connectionId).map(_.message)
    def delete(): Unit = messageStore.delete(connectionId, timestamp)

  "MessageStore" should {
    "fail to store message for unknown connection" in prop { (message: Message) =>
      messageStore.storeInboundWithOutboundMessages(
        newConnectionId,
        Instant.now(),
        message,
        Seq.empty
      ) must throwA[IllegalArgumentException]
    }.set(minTestsOk = 1)

    "fail to store a message for a deleted connection" in prop { (message: Message) =>
      val f = Fixture()
      f.delete()
      f.store(message) must throwA[IllegalArgumentException]
    }.set(minTestsOk = 1)

    "append new messages at the end" in {
      val f = Fixture()
      prop { (message: Message) =>
        f.store(message)

        val loaded = messageStore.findByConnectionId(f.connectionId)
        loaded.map(_.createdAt).lastOption must beSome(f.timestamp)
        loaded.map(_.message).lastOption aka "last appended message" must beSome(message)
      }.set(minTestsOk = 10)
    }

    "store outbound messages after their inbound message" in {
      val f = Fixture()
      prop { (inbound: Message, outbound: Message) =>
        f.store(inbound, outbound)
        f.messages.takeRight(2) must_== Seq(inbound, outbound)
      }.set(minTestsOk = 10)
    }

    "retrieve based on correlation id" in {
      val f = Fixture()
      prop { (message: Message) =>
        withCorrelationId(message) { correlationId =>
          f.store(message)

          val loaded = messageStore.findByCorrelationId(f.requesterNsa, correlationId)
          loaded.map(_.message).lastOption aka "message" must beSome(message)
        }
      }.set(minTestsOk = 10)
    }

    "load messages of live connections only" in prop { (message: Message) =>
      val live, deleted = Fixture()
      Seq(live, deleted).foreach(_.store(message))
      deleted.delete()

      val loaded = messageStore.loadEverything().toMap
      loaded.get(live.connectionId).map(_.last.message) must beSome(message)
      loaded must not(haveKey(deleted.connectionId))
    }.set(minTestsOk = 5)
  }

  private def withCorrelationId(message: Message)(prop: CorrelationId => Prop): Prop =
    MessageConversion(message).toOption.flatMap(_.correlationId).map(prop).getOrElse(Prop.undecided)
end MessageStoreSpecification
