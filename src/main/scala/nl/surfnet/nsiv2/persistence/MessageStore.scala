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
package nl.surfnet.nsiv2
package persistence

import anorm._
import anorm.SqlParser._
import java.sql.Connection
import java.time.Instant
import java.util.UUID
import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.soap._
import nl.surfnet.nsiv2.soap.NsiSoapConversions._
import nl.surfnet.nsiv2.utils._
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType
import play.api.Logger
import play.api.data.validation.ValidationError
import play.api.db.DB
import play.api.libs.json._
import scala.util.{ Try, Success, Failure }

case class MessageData(correlationId: Option[CorrelationId], tpe: String, content: String)
object MessageData {
  def conversionToFormat[A, B: Format](conversion: Conversion[A, B]): Format[A] = new Format[A] {
    override def reads(js: JsValue): JsResult[A] = Json.fromJson[B](js).flatMap { b =>
      conversion.invert(b).toEither.fold(error => JsError(ValidationError("error.conversion.failed", error)), JsSuccess(_))
    }
    override def writes(a: A): JsValue = Json.toJson(conversion(a).get)
  }

  def formatJson[T: Writes](value: T): String = Json.stringify(Json.toJson(value))
  def parseJson[T: Reads](json: String): Try[T] = Json.parse(json).validate[T].fold(errors => Failure(ErrorMessage(errors.mkString(", "))), ok => Success(ok))

  implicit val NsiProviderOperationFormat: Format[NsiProviderMessage[NsiProviderOperation]] = conversionToFormat(NsiProviderMessageToDocument[NsiProviderOperation](None).andThen(DocumentToString))
  implicit val NsiProviderAckFormat: Format[NsiProviderMessage[NsiAcknowledgement]] = conversionToFormat(NsiProviderMessageToDocument[NsiAcknowledgement](None).andThen(DocumentToString))
  implicit val NsiRequesterOperationFormat: Format[NsiRequesterMessage[NsiRequesterOperation]] = conversionToFormat(NsiRequesterMessageToDocument[NsiRequesterOperation](None).andThen(DocumentToString))

  implicit val NsiHeadersFormat: Format[NsiHeaders] = conversionToFormat(Conversion[NsiHeaders, String])
  implicit val ServiceExceptionTypeFormat: Format[ServiceExceptionType] = conversionToFormat(Conversion[ServiceExceptionType, String])
}

case class MessageRecord[T](id: Long, createdAt: Instant, connectionId: ConnectionId, message: T) {
  def map[B](f: T => B) = copy(message = f(message))
}

class MessageStore[M](databaseName: String)(implicit app: play.api.Application, conversion: Conversion[M, MessageData]) {
  def create(connectionId: ConnectionId, createdAt: Instant, requesterNsa: RequesterNsa): Unit = DB.withTransaction(databaseName) { implicit connection =>
    SQL"""
        INSERT INTO connections (connection_id,   created_at,                  requester_nsa)
             VALUES             (${connectionId}, ${createdAt.toSqlTimestamp}, ${requesterNsa})
       """
      .executeInsert()
    ()
  }

  def storeInboundWithOutboundMessages(connectionId: ConnectionId, createdAt: Instant, inbound: M, outbound: Seq[M]) = DB.withTransaction(databaseName) { implicit connection =>
    val connectionPk = SQL"""SELECT id FROM connections WHERE connection_id = ${connectionId} AND deleted_at IS NULL"""
      .as(get[Long]("id").singleOpt)
      .getOrElse {
        throw new IllegalArgumentException(s"connection $connectionId does not exist or is already deleted")
      }
    val inboundId = store(connectionPk, createdAt, inbound, None)
    outbound.foreach(store(connectionPk, createdAt, _, Some(inboundId)))
  }

  def findByConnectionId(connectionId: ConnectionId): Seq[MessageRecord[M]] = DB.withConnection(databaseName) { implicit connection =>
    SQL"""
        SELECT m.id, c.connection_id, m.created_at, m.correlation_id, m.type, m.content
          FROM messages m INNER JOIN connections c ON m.connection_id = c.id
         WHERE c.connection_id = ${connectionId}
         ORDER BY m.id ASC
       """
      .as(recordParser.*)
      .map(_.map(message => conversion.invert(message).get)) // FIXME error handling
  }

  def findByCorrelationId(requesterNsa: String, correlationId: CorrelationId): Seq[MessageRecord[M]] = DB.withConnection(databaseName) { implicit connection =>
    SQL"""
        SELECT m.id, c.connection_id, m.created_at, m.correlation_id, m.type, m.content
          FROM messages m INNER JOIN connections c ON m.connection_id = c.id
         WHERE c.requester_nsa = ${requesterNsa}
           AND m.correlation_id = CAST(${correlationId.value} AS UUID)
         ORDER BY m.id ASC
       """
      .as(recordParser.*)
      .map(_.map(message => conversion.invert(message).get)) // FIXME error handling
  }

  def loadEverything(): Seq[(ConnectionId, Seq[MessageRecord[M]])] = DB.withConnection(databaseName) { implicit connection =>
    SQL"""
        SELECT m.id, c.connection_id, m.created_at, m.correlation_id, m.type, m.content
          FROM messages m INNER JOIN connections c ON m.connection_id = c.id
         WHERE c.deleted_at IS NULL
         ORDER BY m.connection_id ASC, m.id ASC
       """
      .as(recordParser.*)
      .groupBy(_.connectionId)
      .map {
        case (connectionId, records) =>
          connectionId -> records.flatMap { record =>
            val deserialized = conversion.invert(record.message).toOption
            deserialized.map(message => record.map(Function.const(message)))
          }
      }(collection.breakOut)
  }

  def delete(connectionId: ConnectionId, deletedAt: Instant): Unit = DB.withTransaction(databaseName) { implicit connection =>
    SQL"""
        UPDATE connections
           SET deleted_at = ${deletedAt.toSqlTimestamp}
         WHERE connection_id = ${connectionId} AND deleted_at IS NULL
       """
      .executeUpdate().tap(n => Logger.debug(s"Deleted connection $connectionId"))
    ()
  }

  private def recordParser = (get[Long]("id") ~ get[java.util.Date]("created_at") ~ get[String]("connection_id") ~ get[Option[UUID]]("correlation_id") ~ str("type") ~ str("content")).map {
    case id ~ createdAt ~ connectionId ~ correlationId ~ tpe ~ content =>
      MessageRecord(id, createdAt.toInstant(), connectionId, MessageData(correlationId.map(CorrelationId.fromUuid), tpe, content))
  }

  private def store(connectionPk: Long, createdAt: Instant, message: M, inboundId: Option[Long])(implicit connection: Connection) = {
    val serialized = conversion(message).get
    import serialized._
    SQL"""
        INSERT INTO messages (connection_id, correlation_id, type, content, created_at, inbound_message_id)
             VALUES          (${connectionPk}, CAST(${correlationId.map(_.value)} AS UUID), ${tpe}, ${content}, ${createdAt.toSqlTimestamp}, ${inboundId})
       """
      .executeInsert().getOrElse(sys.error("insert failed to generate primary key"))
  }
}
