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
import org.ogf.schemas.nsi._2013._12.connection.types.ReserveType
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.connection.types.ReservationRequestCriteriaType

sealed trait NsiProviderOperation extends NsiOperation {
  def action: String = this.getClass().getSimpleName()
  final def soapActionUrl: String = {
    s"http://schemas.ogf.org/nsi/2013/12/connection/service/${action.uncapitalize}"
  }
}

sealed trait NsiProviderQuery extends NsiProviderOperation
sealed trait NsiProviderCommand extends NsiProviderOperation {
  def optionalConnectionId: Option[ConnectionId]
}
sealed trait NsiProviderUpdateCommand extends NsiProviderCommand {
  def connectionId: ConnectionId
  override def optionalConnectionId: Option[ConnectionId] = Some(connectionId)
}

sealed trait Reserve extends NsiProviderCommand {
  override def action = "Reserve"

  def body: ReserveType

  def criteria: ReservationRequestCriteriaType = body.getCriteria
  def service: Option[P2PServiceBaseType] = criteria.getPointToPointService()
}
case class InitialReserve(body: ReserveType) extends Reserve {
  override def optionalConnectionId: Option[ConnectionId] = None

  require(body.getConnectionId eq null, "initial reserve must not have connectionId")
}
case class ModifyReserve(body: ReserveType) extends Reserve with NsiProviderUpdateCommand {
  def connectionId = body.getConnectionId

  require(connectionId ne null, "modify must have connectionId")
}

case class ReserveCommit(connectionId: ConnectionId) extends NsiProviderUpdateCommand
case class ReserveAbort(connectionId: ConnectionId) extends NsiProviderUpdateCommand

case class Provision(connectionId: ConnectionId) extends NsiProviderUpdateCommand
case class Release(connectionId: ConnectionId) extends NsiProviderUpdateCommand

case class Terminate(connectionId: ConnectionId) extends NsiProviderUpdateCommand

case class QuerySummary(ids: Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]], ifModifiedSince: Option[XMLGregorianCalendar]) extends NsiProviderQuery
case class QuerySummarySync(ids: Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]], ifModifiedSince: Option[XMLGregorianCalendar]) extends NsiProviderQuery
case class QueryRecursive(ids: Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]], ifModifiedSince: Option[XMLGregorianCalendar]) extends NsiProviderQuery

case class QueryNotification(connectionId: ConnectionId, start: Option[Int], end: Option[Int]) extends NsiProviderQuery
case class QueryNotificationSync(connectionId: ConnectionId, start: Option[Int], end: Option[Int]) extends NsiProviderQuery

case class QueryResult(connectionId: ConnectionId, start: Option[Int], end: Option[Int]) extends NsiProviderQuery
case class QueryResultSync(connectionId: ConnectionId, start: Option[Int], end: Option[Int]) extends NsiProviderQuery
