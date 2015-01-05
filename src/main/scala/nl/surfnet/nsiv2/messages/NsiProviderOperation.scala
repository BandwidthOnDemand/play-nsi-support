package nl.surfnet.nsiv2.messages

import org.ogf.schemas.nsi._2013._12.connection.types.{ ReservationConfirmCriteriaType, ReserveType }
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

case class QuerySummary(ids: Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]]) extends NsiProviderQuery
case class QuerySummarySync(ids: Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]]) extends NsiProviderQuery
case class QueryRecursive(ids: Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]]) extends NsiProviderQuery

case class QueryNotification(connectionId: ConnectionId, start: Option[Int], end: Option[Int]) extends NsiProviderQuery
case class QueryNotificationSync(connectionId: ConnectionId, start: Option[Int], end: Option[Int]) extends NsiProviderQuery

case class QueryResult(connectionId: ConnectionId, start: Option[Int], end: Option[Int]) extends NsiProviderQuery
case class QueryResultSync(connectionId: ConnectionId, start: Option[Int], end: Option[Int]) extends NsiProviderQuery
