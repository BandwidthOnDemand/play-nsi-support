package nl.surfnet.nsiv2.messages

import java.net.URI
import net.nordu.namespaces._2013._12.gnsbod.ConnectionType
import nl.surfnet.nsiv2.soap.Conversion
import oasis.names.tc.saml._2_0.assertion.AttributeType
import org.joda.time.DateTime
import org.ogf.schemas.nsi._2013._12.connection.types.ReservationConfirmCriteriaType
import org.ogf.schemas.nsi._2013._12.connection.types.ReservationRequestCriteriaType
import org.ogf.schemas.nsi._2013._12.connection.types.ReserveType
import org.ogf.schemas.nsi._2013._12.connection.types.ScheduleType
import org.ogf.schemas.nsi._2013._12.framework.headers.SessionSecurityAttrType
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.services.types.DirectionalityType
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import scala.util.Try

object Generators {

  implicit val ArbitraryDateTime: Arbitrary[DateTime] = Arbitrary(for {
    year <- Gen.choose(1990, 2050)
    month <- Gen.choose(1, 12)
    day <- Gen.choose(1, 31)
    hour <- Gen.choose(0, 23)
    minute <- Gen.choose(0, 59)
    second <- Gen.choose(0, 59)
    result <- Try(Gen.const(new DateTime(year, month, day, hour, minute, second))) getOrElse Gen.fail[DateTime]
  } yield result)

  implicit val ArbitraryCorrelationId: Arbitrary[CorrelationId] = Arbitrary(for {
    leastSigBits <- arbitrary[Long]
    mostSigBits <- arbitrary[Long]
  } yield CorrelationId(mostSigBits, leastSigBits))

  implicit val ArbitraryURI: Arbitrary[URI] = Arbitrary(Gen.oneOf(
    "https://www.host.invalid/foo/bar", "https://localhost:8443/").map(URI.create))

  implicit val ArbitraryScheduleType: Arbitrary[ScheduleType] = Arbitrary(for {
    startTime <- arbitrary[Option[DateTime]]
    endTime <- arbitrary[Option[DateTime]]
  } yield {
    new ScheduleType()
      .withStartTime(startTime.map(_.toXmlGregorianCalendar).orNull)
      .withEndTime(endTime.map(_.toXmlGregorianCalendar).orNull)
  })

  implicit val ArbitraryP2PServiceBaseType: Arbitrary[P2PServiceBaseType] = Arbitrary(for {
    capacity <- Gen.oneOf(100000L, 500000L, 1000000L, 10000000L)
    directionality <- Gen.oneOf(DirectionalityType.values())
    symmetricPath <- arbitrary[Boolean]
    sourceStp <- Gen.oneOf("source-1", "source-2")
    destStp <- Gen.oneOf("dest-1", "dest-2")
  } yield new P2PServiceBaseType().withCapacity(capacity).withDirectionality(directionality).withSymmetricPath(symmetricPath).withSourceSTP(sourceStp).withDestSTP(destStp))

  implicit val ArbitrarySessionSecurityAttrType: Arbitrary[SessionSecurityAttrType] = Arbitrary(Gen.alphaStr.map { token =>
    new SessionSecurityAttrType().withAttributeOrEncryptedAttribute(new AttributeType().withName("token").withAttributeValue(token))
  })

  implicit val ArbitraryConnectionType: Arbitrary[ConnectionType] = Arbitrary(for {
    value <- Gen.oneOf("connection-type-1", "connection-type-2", "connection-type-3")
    index <- Gen.choose(0, 4)
  } yield new ConnectionType().withValue(value).withIndex(index))

  implicit val ArbitraryReservationRequestCriteriaType: Arbitrary[ReservationRequestCriteriaType] = Arbitrary(for {
    schedule <- arbitrary[ScheduleType]
    serviceType <- Gen.oneOf("service-type-1", "service-type-2")
    service <- arbitrary[P2PServiceBaseType]
  } yield new ReservationRequestCriteriaType().withSchedule(schedule).withServiceType(serviceType).withPointToPointService(service))

  implicit val ArbitraryReservationConfirmCriteriaType: Arbitrary[ReservationConfirmCriteriaType] = Arbitrary(arbitrary[ReservationRequestCriteriaType].flatMap { criteria =>
    Conversion[ReservationConfirmCriteriaType, ReservationRequestCriteriaType].invert(criteria) map Gen.const getOrElse Gen.fail
  })

  implicit val ArbitraryReserveType: Arbitrary[ReserveType] = Arbitrary(for {
    criteria <- arbitrary[ReservationRequestCriteriaType]
  } yield new ReserveType().withCriteria(criteria))

  implicit val ArbitraryNsiHeaders: Arbitrary[NsiHeaders] = Arbitrary(for {
    correlationId <- arbitrary[CorrelationId]
    requesterNsa <- Gen.alphaStr
    providerNsa <- Gen.alphaStr
    replyTo <- arbitrary[Option[URI]]
    protocolVersion <- arbitrary[URI]
    securityAttributes <- Gen.resize(3, Gen.listOf(arbitrary[SessionSecurityAttrType]))
  } yield NsiHeaders(correlationId, requesterNsa, providerNsa, replyTo, protocolVersion, securityAttributes))

  implicit val ArbitraryInitialReserve: Arbitrary[InitialReserve] = Arbitrary(for {
    reserveType <- arbitrary[ReserveType]
    confirmCriteria <- Conversion.invert(reserveType.getCriteria) map Gen.const getOrElse Gen.fail
    service <- reserveType.getCriteria.getPointToPointService map Gen.const getOrElse Gen.fail
  } yield InitialReserve(reserveType, confirmCriteria, service))

  implicit val ArbitraryNsiProviderOperation: Arbitrary[NsiProviderOperation] = Arbitrary(arbitrary[InitialReserve])
  implicit def ArbitraryFromProviderMessage[A <: NsiOperation: Arbitrary]: Arbitrary[NsiProviderMessage[A]] = Arbitrary(Gen.resultOf(NsiProviderMessage.apply[A] _))
}
