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
package nl.surfnet.nsiv2.messages

import javax.xml.datatype.XMLGregorianCalendar
import org.ogf.schemas.nsi._2013._12.connection.types._

sealed trait NsiRequesterOperation extends NsiOperation {
  final def action = this.getClass().getSimpleName()
  final def soapActionUrl: String =
    s"http://schemas.ogf.org/nsi/2013/12/connection/service/${action.uncapitalize}"
}
sealed trait NsiRequesterUpdate extends NsiRequesterOperation {
  def connectionId: ConnectionId;
}
sealed trait NsiNotification extends NsiRequesterUpdate {
  def notification: NotificationBaseType
}
sealed trait NsiCommandReply extends NsiRequesterUpdate

case class ReserveConfirmed(connectionId: ConnectionId, criteria: ReservationConfirmCriteriaType)
    extends NsiCommandReply
case class ReserveFailed(failed: GenericFailedType) extends NsiCommandReply {
  def connectionId: ConnectionId = failed.getConnectionId()
}

case class ReserveCommitConfirmed(connectionId: ConnectionId) extends NsiCommandReply
case class ReserveCommitFailed(failed: GenericFailedType) extends NsiCommandReply {
  def connectionId: ConnectionId = failed.getConnectionId()
}

case class ReserveAbortConfirmed(connectionId: ConnectionId) extends NsiCommandReply

case class ProvisionConfirmed(connectionId: ConnectionId) extends NsiCommandReply
case class ReleaseConfirmed(connectionId: ConnectionId) extends NsiCommandReply
case class TerminateConfirmed(connectionId: ConnectionId) extends NsiCommandReply

case class QuerySummaryConfirmed(
    reservations: Seq[QuerySummaryResultType],
    lastModified: Option[XMLGregorianCalendar]
) extends NsiRequesterOperation
case class QueryRecursiveConfirmed(reservations: Seq[QueryRecursiveResultType])
    extends NsiRequesterOperation
case class QueryNotificationConfirmed(notifications: Seq[NotificationBaseType])
    extends NsiRequesterOperation
case class QueryResultConfirmed(results: Seq[QueryResultResponseType]) extends NsiRequesterOperation

case class ErrorReply(error: GenericErrorType) extends NsiRequesterOperation

case class ErrorEvent(override val notification: ErrorEventType) extends NsiNotification {
  override def connectionId = notification.getConnectionId()
}
case class DataPlaneStateChange(override val notification: DataPlaneStateChangeRequestType)
    extends NsiNotification {
  def connectionId: ConnectionId = notification.getConnectionId()
}
case class ReserveTimeout(override val notification: ReserveTimeoutRequestType)
    extends NsiNotification {
  def connectionId: ConnectionId = notification.getConnectionId()
}

case class MessageDeliveryTimeout(override val notification: MessageDeliveryTimeoutRequestType)
    extends NsiNotification {
  def connectionId: ConnectionId = notification.getConnectionId()
}
