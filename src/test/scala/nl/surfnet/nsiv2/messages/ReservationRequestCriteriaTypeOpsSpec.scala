package nl.surfnet.nsiv2.messages

import Generators.*
import javax.xml.datatype.XMLGregorianCalendar
import nl.surfnet.bod.nsi.Nillable
import org.ogf.schemas.nsi._2013._12.connection.types.{
  ReservationConfirmCriteriaType,
  ReservationRequestCriteriaType
}

class ReservationRequestCriteriaTypeOpsSpec
    extends org.specs2.mutable.Specification
    with org.specs2.ScalaCheck {

  "conversion to confirm criteria" should {
    "initial criteria" >> {
      "use specified STPs" in prop {
        (request: ReservationRequestCriteriaType, source: Stp, dest: Stp) =>
          val service = request
            .toInitialConfirmCriteria(source.toString, dest.toString)
            .get
            .pointToPointService
            .get
          service.getSourceSTP must_== source.toString
          service.getDestSTP must_== dest.toString
      }

      "use specified schedule" in prop { (request: ReservationRequestCriteriaType) =>
        val schedule = request.toInitialConfirmCriteria("A", "B").get.getSchedule
        schedule must not(beNull)
        schedule must_== request.getSchedule
      }
    }

    "modified criteria" >> {
      "default missing schedule to confirm criteria" in prop {
        (request: ReservationRequestCriteriaType, confirm: ReservationConfirmCriteriaType) =>
          request.setSchedule(null)

          request.toModifiedConfirmCriteria(confirm).get.getSchedule must_== confirm.getSchedule
      }

      "default missing start time to confirm criteria" in prop {
        (request: ReservationRequestCriteriaType, confirm: ReservationConfirmCriteriaType) =>
          request.getSchedule.withStartTime(Nillable.absent[XMLGregorianCalendar])

          request
            .toModifiedConfirmCriteria(confirm)
            .get
            .getSchedule
            .getStartTime must_== confirm.getSchedule.getStartTime
      }

      "overwrite null end time" in prop {
        (request: ReservationRequestCriteriaType, confirm: ReservationConfirmCriteriaType) =>
          request.getSchedule.withEndTime(Nillable.absent[XMLGregorianCalendar])

          request.toModifiedConfirmCriteria(confirm).get.getSchedule.getEndTime must_== Nillable
            .absent[XMLGregorianCalendar]
      }

      "STPs" >> {
        "copy STPs from committed version" in prop {
          (request: ReservationRequestCriteriaType, confirm: ReservationConfirmCriteriaType) =>
            val committed = confirm.pointToPointService.get
            val modified = request.toModifiedConfirmCriteria(confirm).get.pointToPointService.get
            modified.getSourceSTP must_== committed.getSourceSTP
            modified.getDestSTP must_== committed.getDestSTP
        }
      }

      "version" >> {
        "keep specified value" in prop {
          (request: ReservationRequestCriteriaType, confirm: ReservationConfirmCriteriaType) =>
            (request.getVersion ne null) ==> {
              request.toModifiedConfirmCriteria(confirm).get.getVersion must_== request.getVersion
            }
        }

        "set unspecified version to previous committed version plus one" in prop {
          (request: ReservationRequestCriteriaType, confirm: ReservationConfirmCriteriaType) =>
            request.setVersion(null)

            request.toModifiedConfirmCriteria(confirm).get.getVersion must_== confirm.getVersion + 1
        }
      }

      "serviceType" >> {
        "keep committed value" in prop {
          (request: ReservationRequestCriteriaType, confirm: ReservationConfirmCriteriaType) =>
            request
              .toModifiedConfirmCriteria(confirm)
              .get
              .getServiceType must_== confirm.getServiceType
        }
      }
    }
  }
}
