package nl.surfnet.nsiv2.messages

import java.net.URI
import java.time.Instant
import javax.xml.datatype.XMLGregorianCalendar
import net.nordu.namespaces._2013._12.gnsbod.ConnectionType
import nl.surfnet.bod.nsi.Nillable
import nl.surfnet.nsiv2.soap.Conversion
import nl.surfnet.nsiv2.utils._
import oasis.names.tc.saml._2_0.assertion.AttributeType
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

object Generators {

  implicit val ArbitraryInstant: Arbitrary[Instant] = Arbitrary(for {
    timeInMillis <- Gen.choose(0, System.currentTimeMillis() * 3)
  } yield Instant.ofEpochMilli(timeInMillis))

  implicit val ArbitraryCorrelationId: Arbitrary[CorrelationId] = Arbitrary(for {
    leastSigBits <- arbitrary[Long]
    mostSigBits <- arbitrary[Long]
  } yield CorrelationId(mostSigBits, leastSigBits))

  implicit val ArbitraryURI: Arbitrary[URI] = Arbitrary(Gen.oneOf(
    "https://www.host.invalid/foo/bar", "https://localhost:8443/").map(URI.create))

  implicit val ArbitraryScheduleType: Arbitrary[ScheduleType] = Arbitrary(for {
    startTime <- arbitrary[Nillable[XMLGregorianCalendar]]
    endTime <- arbitrary[Nillable[XMLGregorianCalendar]]
  } yield {
    new ScheduleType()
      .withStartTime(startTime)
      .withEndTime(endTime)
  })

  implicit val ArbitraryXMLGregorianCalendar: Arbitrary[XMLGregorianCalendar] = Arbitrary(for {
    now <- arbitrary[Instant]
  } yield now.toXMLGregorianCalendar() )

  implicit val ArbitraryStp: Arbitrary[Stp] = Arbitrary(Gen.oneOf("STP-A", "STP-B", "STP-C").flatMap { s => Stp.fromString(s) map Gen.const getOrElse Gen.fail })

  implicit def ArbitraryNillable[A](implicit a: Arbitrary[A]): Arbitrary[Nillable[A]] = Arbitrary {
    for {
      x <- arbitrary[A]
      result <- Gen.oneOf(Nillable.present(x), Nillable.absent[A], Nillable.nil[A])
    } yield result
  }

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
    version <- arbitrary[Option[Int]]
  } yield new ReservationRequestCriteriaType()
    .withSchedule(schedule)
    .withServiceType(serviceType)
    .withPointToPointService(service)
    .withVersion(version.map(Integer.valueOf).orNull))

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

  implicit val ArbitraryInitialReserve: Arbitrary[InitialReserve] = Arbitrary(Gen.resultOf(InitialReserve.apply _))

  implicit val ArbitraryNsiProviderOperation: Arbitrary[NsiProviderOperation] = Arbitrary(arbitrary[InitialReserve])
  implicit def ArbitraryFromProviderMessage[A <: NsiOperation: Arbitrary]: Arbitrary[NsiProviderMessage[A]] = Arbitrary(Gen.resultOf(NsiProviderMessage.apply[A] _))
}
