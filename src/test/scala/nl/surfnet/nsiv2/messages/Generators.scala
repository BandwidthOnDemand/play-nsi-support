package nl.surfnet.nsiv2.messages

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.ogf.schemas.nsi._2013._12.connection.types.ScheduleType
import scala.util.Try
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.services.types.DirectionalityType
import net.nordu.namespaces._2013._12.gnsbod.ConnectionType
import org.joda.time.DateTime
import java.net.URI

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
}
