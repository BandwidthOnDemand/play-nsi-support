/*
 * Copyright (c) 2012, 2013, 2014, 2015, 2016 SURFnet BV
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
package nl.surfnet.nsiv2.utils

import org.ogf.schemas.nsi._2013._12.connection.types.ReservationRequestCriteriaType
import org.ogf.schemas.nsi._2013._12.connection.types.ReservationConfirmCriteriaType

final case class ConnectionCriteria(
    pending: Option[Either[ReservationRequestCriteriaType, ReservationConfirmCriteriaType]],
    committed: Option[ReservationConfirmCriteriaType]
) {
  def withRequested(requested: ReservationRequestCriteriaType): ConnectionCriteria =
    copy(pending = Some(Left(requested)))
  def withHeld(held: ReservationConfirmCriteriaType): ConnectionCriteria =
    copy(pending = Some(Right(held)))

  def commit: ConnectionCriteria = copy(pending = None, committed = pending.flatMap(_.toOption))
  def abort: ConnectionCriteria = copy(pending = None)

  def requested: Option[ReservationRequestCriteriaType] = pending.flatMap(_.left.toOption)
  def confirmed: Option[ReservationConfirmCriteriaType] = pending.flatMap(_.toOption)

  def pendingVersion: Int = {
    pending
      .flatMap(
        _.fold(
          requested =>
            if requested.getVersion eq null then None else Some(requested.getVersion.intValue),
          confirmed => Some(confirmed.getVersion)
        )
      )
      .orElse(committed.map(_.getVersion + 1))
      .getOrElse(1)
  }
}
object ConnectionCriteria {
  val Initial: ConnectionCriteria = ConnectionCriteria(None, None)
}
