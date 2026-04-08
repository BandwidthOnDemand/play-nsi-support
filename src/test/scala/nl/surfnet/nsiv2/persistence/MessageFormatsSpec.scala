package nl.surfnet.nsiv2.persistence

import java.net.URI
import nl.surfnet.nsiv2.messages.{given, *}
import nl.surfnet.nsiv2.soap.NsiSoapConversions.{given, *}
import org.ogf.schemas.nsi._2013._12.connection.types.*
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.services.types.DirectionalityType
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import play.api.libs.json.*
import scala.util.Success

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class MessageFormatsSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck:
  import Generators.given

  val testHeaders: NsiHeaders = NsiHeaders(
    CorrelationId.random(),
    "urn:ogf:network:requester.tld:nsa",
    "urn:ogf:network:provider.tld:nsa",
    Some(URI.create("http://localhost:9000/reply")),
    NsiHeaders.ProviderProtocolVersion
  )

  def testReserveType: ReserveType = new ReserveType()
    .withCriteria(
      new ReservationRequestCriteriaType()
        .withVersion(1)
        .withSchedule(new ScheduleType())
        .withServiceType("http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE")
        .withPointToPointService(
          new P2PServiceBaseType()
            .withCapacity(100)
            .withDirectionality(DirectionalityType.BIDIRECTIONAL)
            .withSourceSTP("urn:stp:a")
            .withDestSTP("urn:stp:b")
        )
    )

  "NsiProviderOperationFormat" should {
    "round-trip InitialReserve message through JSON" in {
      val msg = NsiProviderMessage(testHeaders, InitialReserve(testReserveType))

      val json = Json.toJson(msg)(NsiProviderOperationFormat)
      json must beAnInstanceOf[JsString]

      val parsed = Json.fromJson[NsiProviderMessage[NsiProviderOperation]](json)(NsiProviderOperationFormat)
      parsed must beLike { case JsSuccess(m, _) =>
        m.headers.correlationId must beEqualTo(testHeaders.correlationId)
        m.headers.requesterNSA must beEqualTo(testHeaders.requesterNSA)
        m.headers.providerNSA must beEqualTo(testHeaders.providerNSA)
        m.body must beAnInstanceOf[InitialReserve]
      }
    }

    "round-trip arbitrary provider messages through JSON" in {
      prop { (msg: NsiProviderMessage[NsiProviderOperation]) =>
        val json = Json.toJson(msg)(NsiProviderOperationFormat)
        val parsed = Json.fromJson[NsiProviderMessage[NsiProviderOperation]](json)(NsiProviderOperationFormat)
        parsed must beLike { case JsSuccess(m, _) =>
          m.headers.correlationId must beEqualTo(msg.headers.correlationId)
          m.body.getClass must beEqualTo(msg.body.getClass)
        }
      }.set(minTestsOk = 10)
    }
  }

  "NsiRequesterOperationFormat" should {
    "round-trip ReserveConfirmed message through JSON" in {
      val confirmCriteria = new ReservationConfirmCriteriaType()
        .withVersion(1)
        .withSchedule(new ScheduleType())
        .withServiceType("ServiceType")
        .withPointToPointService(
          new P2PServiceBaseType()
            .withCapacity(100)
            .withDirectionality(DirectionalityType.BIDIRECTIONAL)
            .withSourceSTP("urn:stp:a")
            .withDestSTP("urn:stp:b")
        )
      val msg = NsiRequesterMessage(
        testHeaders.copy(protocolVersion = NsiHeaders.RequesterProtocolVersion),
        ReserveConfirmed("connection-id-1", confirmCriteria)
      )

      val json = Json.toJson(msg)(NsiRequesterOperationFormat)
      json must beAnInstanceOf[JsString]

      val parsed = Json.fromJson[NsiRequesterMessage[NsiRequesterOperation]](json)(NsiRequesterOperationFormat)
      parsed must beLike { case JsSuccess(m, _) =>
        m.headers.correlationId must beEqualTo(testHeaders.correlationId)
        m.body must beAnInstanceOf[ReserveConfirmed]
        m.body.asInstanceOf[ReserveConfirmed].connectionId must beEqualTo("connection-id-1")
      }
    }
  }

  "NsiProviderAckFormat" should {
    "round-trip GenericAck through JSON" in {
      val msg = NsiProviderMessage(testHeaders, GenericAck())

      val json = Json.toJson(msg)(NsiProviderAckFormat)
      json must beAnInstanceOf[JsString]

      val parsed = Json.fromJson[NsiProviderMessage[NsiAcknowledgement]](json)(NsiProviderAckFormat)
      parsed must beLike { case JsSuccess(m, _) =>
        m.headers.correlationId must beEqualTo(testHeaders.correlationId)
        m.body must beAnInstanceOf[GenericAck]
      }
    }
  }

  "NsiHeadersFormat" should {
    "round-trip headers with replyTo through JSON" in {
      val json = Json.toJson(testHeaders)(NsiHeadersFormat)
      json must beAnInstanceOf[JsString]

      val parsed = Json.fromJson[NsiHeaders](json)(NsiHeadersFormat)
      parsed must beLike { case JsSuccess(h, _) =>
        h.correlationId must beEqualTo(testHeaders.correlationId)
        h.requesterNSA must beEqualTo(testHeaders.requesterNSA)
        h.providerNSA must beEqualTo(testHeaders.providerNSA)
        h.replyTo must beEqualTo(testHeaders.replyTo)
      }
    }

    "round-trip headers without replyTo through JSON" in {
      val headers = testHeaders.copy(replyTo = None)
      val json = Json.toJson(headers)(NsiHeadersFormat)

      val parsed = Json.fromJson[NsiHeaders](json)(NsiHeadersFormat)
      parsed must beLike { case JsSuccess(h, _) =>
        h.replyTo must beNone
      }
    }

    "round-trip arbitrary headers through JSON" in {
      prop { (headers: NsiHeaders) =>
        val json = Json.toJson(headers)(NsiHeadersFormat)
        val parsed = Json.fromJson[NsiHeaders](json)(NsiHeadersFormat)
        parsed must beLike { case JsSuccess(h, _) =>
          h.correlationId must beEqualTo(headers.correlationId)
          h.requesterNSA must beEqualTo(headers.requesterNSA)
          h.providerNSA must beEqualTo(headers.providerNSA)
        }
      }.set(minTestsOk = 20)
    }
  }

  "MessageData helpers" should {
    "formatJson and parseJson round-trip for Instant" in {
      val instant = java.time.Instant.now()
      val json = MessageData.formatJson(instant)
      val parsed = MessageData.parseJson[java.time.Instant](json)
      parsed must beLike { case Success(i) =>
        i.toEpochMilli must beEqualTo(instant.toEpochMilli)
      }
    }

    "formatJson and parseJson round-trip for CorrelationId" in {
      val corrId = CorrelationId.random()
      val json = MessageData.formatJson(corrId)
      val parsed = MessageData.parseJson[CorrelationId](json)
      parsed must beLike { case Success(c) =>
        c must beEqualTo(corrId)
      }
    }

    "formatJson and parseJson round-trip for URI" in {
      val uri = java.net.URI.create("https://example.com/endpoint")
      val json = MessageData.formatJson(uri)
      val parsed = MessageData.parseJson[java.net.URI](json)
      parsed must beLike { case Success(u) =>
        u must beEqualTo(uri)
      }
    }

    "parseJson throws on invalid JSON" in {
      MessageData.parseJson[CorrelationId]("not-json") must throwA[com.fasterxml.jackson.core.JsonParseException]
    }

    "parseJson returns Failure for wrong type" in {
      val parsed = MessageData.parseJson[CorrelationId]("42")
      parsed must beFailedTry
    }
  }
end MessageFormatsSpec
