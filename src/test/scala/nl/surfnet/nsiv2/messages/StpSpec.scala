package nl.surfnet.nsiv2.messages

import org.ogf.schemas.nsi._2013._12.connection.types.{ ScheduleType, ReservationRequestCriteriaType }
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import com.google.common.collect.Range
import scala.math.Ordering.Implicits._
import scala.collection.immutable.SortedMap
import org.specs2.matcher.{ Matcher, MatchResult }

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class StpSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck { outer =>
  private def stp(s: String): Stp = Stp.fromString(s).get
  private def vlan(s: String): VlanRange = VlanRange.fromString(s).get

  private val GenVlanId = Gen.chooseNum(2, 4095).map(Integer.valueOf)

  private implicit val ArbitraryStp: Arbitrary[Stp] = Arbitrary(for {
    identifier <- arbitrary[String] if identifier.nonEmpty
    labels <- arbitrary[Seq[Stp.Label]] if labels.forall(_._1.nonEmpty)
  } yield Stp(identifier, SortedMap(labels: _*)))

  private implicit val ArbitraryVlanRange: Arbitrary[VlanRange] = Arbitrary(
    Gen.nonEmptyListOf(for (a <- GenVlanId; b <- GenVlanId) yield Range.closed(a min b, a max b)).map(VlanRange.apply))

  private def beCompatibleWith(target: Stp): Matcher[Stp] = { source: Stp =>
    (source isCompatibleWith target, s"$source is compatible with $target", s"$source is not compatible with $target")
  }

  "VlanRange" should {
    "print and parse" in prop { vlanRange: VlanRange =>
      VlanRange.fromString(vlanRange.toString) must beSome(vlanRange)
    }

    "fail to parse empty range" in {
      VlanRange.fromString("10-9") must beNone
    }

    "parse singleton" in {
      VlanRange.fromString("3") must beSome.which(_.isSingleton)
    }

    "calculate intersection" in {
      vlan("10-20") intersect vlan("18-30") must beSome(vlan("18-20"))
    }
  }

  "STP" should {
    "fail to parse empty STP" in {
      Stp.fromString("") must beNone
      Stp.fromString("?") must beNone
      Stp.fromString("?label=foo") must beNone
      Stp.fromString("identifer?=foo") must beNone
    }
    "parse without label" in {
      Stp.fromString("urn:ogf:network:surfnet.nl:1990:testbed:00:03:18:c3:1e:00-9-4") must beSome(Stp("urn:ogf:network:surfnet.nl:1990:testbed:00:03:18:c3:1e:00-9-4"))
    }
    "parse with labelType" in {
      Stp.fromString("urn:ogf:network:surfnet.nl:1990:testbed:00:03:18:c3:1e:00-9-4?vlan") must beSome(Stp("urn:ogf:network:surfnet.nl:1990:testbed:00:03:18:c3:1e:00-9-4", SortedMap("vlan" -> None)))
    }
    "parse with labelType and labelValue" in {
      Stp.fromString("urn:ogf:network:surfnet.nl:1990:testbed:00:03:18:c3:1e:00-9-4?vlan=2-1000") must beSome(Stp("urn:ogf:network:surfnet.nl:1990:testbed:00:03:18:c3:1e:00-9-4", SortedMap("vlan" -> Some("2-1000"))))
    }
    "parse with multiple labels" in {
      Stp.fromString("identifier?vlan=1-200&protected&s-vlan=1-1000") must beSome(Stp("identifier", SortedMap("vlan" -> Some("1-200"), "protected" -> None, "s-vlan" -> Some("1-1000"))))
    }

    "parse with embedded question mark" in {
      Stp.fromString(Stp("id?ent").toString) must beSome(Stp("id?ent"))
    }

    "parse any stringified STP" in prop { stp: Stp =>
      Stp.fromString(stp.toString) must beSome(stp)
    }
  }

  "STP without VLAN" should {
    "be compatible with same STP" in {
      stp("urn:ogf:network:a") must beCompatibleWith(stp("urn:ogf:network:a"))
    }
    "not be compatible with STP with different identifier" in {
      stp("urn:ogf:network:a") must not(beCompatibleWith(stp("urn:ogf:network:b")))
    }
    "be compatible with VLAN STP" in {
      stp("urn:ogf:network:a") must beCompatibleWith(stp("urn:ogf:network:a?vlan=10-100"))
    }
  }

  "STP with C-VLAN" should {
    "not be compatible STP without VLAN" in {
      stp("urn:ogf:network:a?vlan=10") must not(beCompatibleWith(stp("urn:ogf:network:a")))
    }

    "not be compatible with STP that has non-containing VLAN range" in {
      stp("urn:ogf:network:a?vlan=10") must not(beCompatibleWith(stp("urn:ogf:network:a?vlan=20-30")))
    }

    "be compatible with STP that has containing VLAN range" in {
      stp("urn:ogf:network:a?vlan=10") must beCompatibleWith(stp("urn:ogf:network:a?vlan=10-30"))
    }
  }

  "STP without S-VLAN" should {
    "not be compatible with S-VLAN STP" in {
      stp("urn:ogf:network:a") must not(beCompatibleWith(stp("urn:ogf:network:a?s-vlan=10-100")))
    }
  }

  "STP with S-VLAN" should {
    "not be compatible STP without S-VLAN" in {
      stp("urn:ogf:network:a?s-vlan=10") must not(beCompatibleWith(stp("urn:ogf:network:a")))
    }

    "not be compatible with C-VLAN STP" in {
      stp("urn:ogf:network:a?s-vlan=10") must not(beCompatibleWith(stp("urn:ogf:network:a?vlan=10")))
    }

    "not be compatible with STP that has non-containing S-VLAN range" in {
      stp("urn:ogf:network:a?s-vlan=10") must not(beCompatibleWith(stp("urn:ogf:network:a?s-vlan=20-30")))
    }

    "be compatible with STP that has containing VLAN range" in {
      stp("urn:ogf:network:a?s-vlan=10") must beCompatibleWith(stp("urn:ogf:network:a?s-vlan=10-30"))
    }
  }
}
