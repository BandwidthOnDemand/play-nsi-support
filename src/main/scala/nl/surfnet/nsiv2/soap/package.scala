package nl.surfnet.nsiv2

import org.ogf.schemas.nsi._2013._12.connection.types.ScheduleType
import org.ogf.schemas.nsi._2013._12.connection.types.{ReservationRequestCriteriaType, ReservationConfirmCriteriaType}
import scala.util.{Try, Success, Failure}
import utils._

package object soap {
  // FIXME
  implicit val ReservationCriteriaConversion = Conversion.build[ReservationConfirmCriteriaType, ReservationRequestCriteriaType] { a =>
    Try(new ReservationRequestCriteriaType().
      withSchedule(a.getSchedule).
      withAny(a.getAny).
      withServiceType(a.getServiceType).
      withVersion(a.getVersion).
      tap(_.getOtherAttributes.putAll(a.getOtherAttributes)))
  } { b =>
    for {
      serviceType <- Option(b.getServiceType).map(Success(_)).getOrElse(Failure(ErrorMessage("serviceType is required")))
    } yield {
      val schedule = Option(b.getSchedule).getOrElse(new ScheduleType())
      new ReservationConfirmCriteriaType().
        withSchedule(schedule).
        withAny(b.getAny).
        withServiceType(b.getServiceType).
        withVersion(if (b.getVersion == null) 1 else b.getVersion).
        tap(_.getOtherAttributes.putAll(b.getOtherAttributes))
    }
  }
}
