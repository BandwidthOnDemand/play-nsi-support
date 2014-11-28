package nl.surfnet.nsiv2.messages

import org.specs2._

import org.ogf.schemas.nsi._2013._12.connection.types.{ScheduleType, ReservationRequestCriteriaType}

@org.junit.runner.RunWith(classOf[runner.JUnitRunner])
class StpSpec extends mutable.Specification {
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
}
