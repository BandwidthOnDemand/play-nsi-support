package nl.surfnet.nsiv2.messages

import java.net.URI
import java.time.Instant
import javax.xml.datatype.XMLGregorianCalendar
import net.nordu.namespaces._2013._12.gnsbod.ConnectionType
import nl.surfnet.bod.nsi.Nillable
import nl.surfnet.nsiv2.utils.*
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

object Generators:

  given Arbitrary[Instant] = Arbitrary(
    for timeInMillis <- Gen.choose(0L, System.currentTimeMillis() * 3)
    yield Instant.ofEpochMilli(timeInMillis)
  )

  given Arbitrary[CorrelationId] = Arbitrary(for
    leastSigBits <- arbitrary[Long]
    mostSigBits <- arbitrary[Long]
  yield CorrelationId(mostSigBits, leastSigBits))

  given Arbitrary[URI] = Arbitrary(
    Gen.oneOf("https://www.host.invalid/foo/bar", "https://localhost:8443/").map(URI.create)
  )

  given Arbitrary[ScheduleType] = Arbitrary(
    for
      startTime <- arbitrary[Nillable[XMLGregorianCalendar]]
      endTime <- arbitrary[Nillable[XMLGregorianCalendar]]
    yield new ScheduleType()
      .withStartTime(startTime)
      .withEndTime(endTime)
  )

  given Arbitrary[XMLGregorianCalendar] = Arbitrary(
    for now <- arbitrary[Instant]
    yield now.toXMLGregorianCalendar()
  )

  given Arbitrary[Stp] = Arbitrary(
    Gen.oneOf("STP-A", "STP-B", "STP-C").flatMap { s =>
      Stp.fromString(s) map Gen.const getOrElse Gen.fail
    }
  )

  given [A: Arbitrary]: Arbitrary[Nillable[A]] = Arbitrary {
    for
      x <- arbitrary[A]
      result <- Gen.oneOf(Nillable.present(x), Nillable.absent[A], Nillable.nil[A])
    yield result
  }

  given Arbitrary[P2PServiceBaseType] = Arbitrary(
    for
      capacity <- Gen.oneOf(100000L, 500000L, 1000000L, 10000000L)
      directionality <- Gen.oneOf(DirectionalityType.values().toIndexedSeq)
      symmetricPath <- arbitrary[Boolean]
      sourceStp <- Gen.oneOf("source-1", "source-2")
      destStp <- Gen.oneOf("dest-1", "dest-2")
    yield new P2PServiceBaseType()
      .withCapacity(capacity)
      .withDirectionality(directionality)
      .withSymmetricPath(symmetricPath)
      .withSourceSTP(sourceStp)
      .withDestSTP(destStp)
  )

  given Arbitrary[SessionSecurityAttrType] = Arbitrary(
    Gen.alphaStr.map { token =>
      new SessionSecurityAttrType().withAttributeOrEncryptedAttribute(
        new AttributeType().withName("token").withAttributeValue(token)
      )
    }
  )

  given Arbitrary[ConnectionType] = Arbitrary(for
    value <- Gen.oneOf("connection-type-1", "connection-type-2", "connection-type-3")
    index <- Gen.choose(0, 4)
  yield new ConnectionType().withValue(value).withIndex(index))

  given Arbitrary[ReservationRequestCriteriaType] =
    Arbitrary(
      for
        schedule <- arbitrary[ScheduleType]
        serviceType <- Gen.oneOf("service-type-1", "service-type-2")
        service <- arbitrary[P2PServiceBaseType]
        version <- arbitrary[Option[Int]]
      yield new ReservationRequestCriteriaType()
        .withSchedule(schedule)
        .withServiceType(serviceType)
        .withPointToPointService(service)
        .withVersion(version.map(Integer.valueOf).orNull)
    )

  given Arbitrary[ReservationConfirmCriteriaType] =
    Arbitrary(
      for
        schedule <- arbitrary[ScheduleType]
        serviceType <- Gen.oneOf("service-type-1", "service-type-2")
        service <- arbitrary[P2PServiceBaseType]
        version <- arbitrary[Int]
      yield new ReservationConfirmCriteriaType()
        .withSchedule(schedule)
        .withServiceType(serviceType)
        .withPointToPointService(service)
        .withVersion(version)
    )

  given Arbitrary[ReserveType] = Arbitrary(
    for criteria <- arbitrary[ReservationRequestCriteriaType]
    yield new ReserveType().withCriteria(criteria)
  )

  given Arbitrary[NsiHeaders] = Arbitrary(
    for
      correlationId <- arbitrary[CorrelationId]
      requesterNsa <- Gen.alphaStr
      providerNsa <- Gen.alphaStr
      replyTo <- arbitrary[Option[URI]]
      protocolVersion <- arbitrary[URI]
      securityAttributes <- Gen.resize(3, Gen.listOf(arbitrary[SessionSecurityAttrType]))
    yield NsiHeaders(
      correlationId,
      requesterNsa,
      providerNsa,
      replyTo,
      protocolVersion,
      securityAttributes
    )
  )

  given Arbitrary[InitialReserve] = Arbitrary(Gen.resultOf(InitialReserve.apply _))

  given Arbitrary[NsiProviderOperation] = Arbitrary(arbitrary[InitialReserve])

  given [A <: NsiOperation: Arbitrary]: Arbitrary[NsiProviderMessage[A]] = Arbitrary(
    Gen.resultOf(NsiProviderMessage.apply[A] _)
  )
end Generators
