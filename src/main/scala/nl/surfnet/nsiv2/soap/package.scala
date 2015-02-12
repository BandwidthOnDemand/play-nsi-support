/*
 * Copyright (c) 2012, 2013, 2014, 2015 SURFnet BV
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
 * following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
 *     disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided with the distribution.
 *   * Neither the name of the SURFnet BV nor the names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
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
