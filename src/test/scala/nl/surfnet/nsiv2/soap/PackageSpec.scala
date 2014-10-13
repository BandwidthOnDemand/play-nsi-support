package nl.surfnet.nsiv2.soap

import org.specs2._

import org.ogf.schemas.nsi._2013._12.connection.types.{ScheduleType, ReservationRequestCriteriaType}

@org.junit.runner.RunWith(classOf[runner.JUnitRunner])
class PackageSpec extends mutable.Specification {
  "ReservationCriteriaConversion conversion" should {
    "default the version to 1" in {
      val reserveCriteria = new ReservationRequestCriteriaType().withSchedule(new ScheduleType()).withServiceType("servicetype")
      val confirmCriteria = ReservationCriteriaConversion.invert(reserveCriteria)

      confirmCriteria must beSuccessfulTry.which(_.getVersion == 1)
    }
  }
}
