package nl.surfnet.nsiv2.messages

import org.specs2._
import org.ogf.schemas.nsi._2013._12.connection.types.{ ScheduleType, ReservationRequestCriteriaType }
import org.specs2.matcher.ScalaCheckMatchers
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import com.google.common.collect.Range
import scala.math.Ordering.Implicits._

@org.junit.runner.RunWith(classOf[runner.JUnitRunner])
class StpSpec extends mutable.Specification with ScalaCheckMatchers {
  private def stp(s: String): Stp = Stp.fromString(s).get

  private val GenVlanId = Gen.chooseNum(2, 4095).map(Integer.valueOf)

  private implicit val ArbitraryVlanRange: Arbitrary[VlanRange] = Arbitrary(
    Gen.nonEmptyListOf(for (a <- GenVlanId; b <- GenVlanId) yield Range.closed(a min b, a max b)).map(VlanRange.apply))

  "VlanRange" should {
    "print and parse" in prop { vlanRange: VlanRange =>
      VlanRange.fromString(vlanRange.toString) must beSome(vlanRange)
    }
  }

  "STP" should {
    "parse without label" in {
      Stp.fromString("urn:ogf:network:surfnet.nl:1990:testbed:00:03:18:c3:1e:00-9-4") must beSome(Stp("urn:ogf:network:surfnet.nl:1990:testbed:00:03:18:c3:1e:00-9-4"))
    }
    "parse with labelType" in {
      Stp.fromString("urn:ogf:network:surfnet.nl:1990:testbed:00:03:18:c3:1e:00-9-4?vlan") must beSome(Stp("urn:ogf:network:surfnet.nl:1990:testbed:00:03:18:c3:1e:00-9-4", Some(Stp.Label("vlan", None))))
    }
    "parse with labelType and labelValue" in {
      Stp.fromString("urn:ogf:network:surfnet.nl:1990:testbed:00:03:18:c3:1e:00-9-4?vlan=2-1000") must beSome(Stp("urn:ogf:network:surfnet.nl:1990:testbed:00:03:18:c3:1e:00-9-4", Some(Stp.Label("vlan", Some("2-1000")))))
    }

  }

  "STP without VLAN" should {
    "be compatible with same STP" in {
      stp("urn:ogf:network:a") isCompatibleWith stp("urn:ogf:network:a") must beTrue
    }
    "not be compatible with STP with different identifier" in {
      stp("urn:ogf:network:a") isCompatibleWith stp("urn:ogf:network:b") must beFalse
    }
    "be compatible with VLAN STP" in {
      stp("urn:ogf:network:a") isCompatibleWith stp("urn:ogf:network:a?vlan=10-100") must beTrue
    }
  }

  "STP with VLAN" should {
    "not be compatible STP without VLAN" in {
      stp("urn:ogf:network:a?vlan=10") isCompatibleWith stp("urn:ogf:network:a") must beFalse
    }

    "not be compatible with STP that has non-containing VLAN range" in {
      stp("urn:ogf:network:a?vlan=10") isCompatibleWith stp("urn:ogf:network:a?vlan=20-30") must beFalse
    }

    "be compatible with STP that has containing VLAN range" in {
      stp("urn:ogf:network:a?vlan=10") isCompatibleWith stp("urn:ogf:network:a?vlan=10-30") must beTrue
    }
  }
}
