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

import java.net.URI
import javax.xml.namespace.QName

import net.nordu.namespaces._2013._12.gnsbod.{ ConnectionType, ConnectionTraceType }

import org.ogf.schemas.nsi._2013._12.framework.headers.SessionSecurityAttrType
import org.ogf.schemas.nsi._2013._12.framework.types._
import org.ogf.schemas.nsi._2015._04.connection.pathtrace.PathTraceType

import scala.collection.JavaConverters._

object NsiHeaders {
  val ProviderProtocolVersion: URI = URI.create("application/vnd.ogf.nsi.cs.v2.provider+soap")
  val RequesterProtocolVersion: URI = URI.create("application/vnd.ogf.nsi.cs.v2.requester+soap")

  val PROVIDER_NSA = new QName(QNAME_NSI_HEADERS.getNamespaceURI, "providerNSA")
  val REQUESTER_NSA = new QName(QNAME_NSI_HEADERS.getNamespaceURI, "requesterNSA")

  private val gnsFactory = new net.nordu.namespaces._2013._12.gnsbod.ObjectFactory()
  private val pathTraceFactory = new org.ogf.schemas.nsi._2015._04.connection.pathtrace.ObjectFactory()

  private val NullConnectionTraceElement = gnsFactory.createConnectionTrace(null)
  private val NullPathTraceElement = pathTraceFactory.createPathTrace(null)
}
case class NsiHeaders(
    correlationId: CorrelationId,
    requesterNSA: RequesterNsa,
    providerNSA: String,
    replyTo: Option[URI],
    protocolVersion: URI,
    sessionSecurityAttrs: List[SessionSecurityAttrType] = Nil,
    any: XmlAny = XmlAny.empty,
    otherAttributes: Map[QName, String] = Map.empty
) {
  def forSyncAck: NsiHeaders = NsiHeaders(correlationId, requesterNSA, providerNSA, None, protocolVersion)

  def forAsyncReply: NsiHeaders = NsiHeaders(
    correlationId,
    requesterNSA,
    providerNSA,
    None,
    NsiHeaders.RequesterProtocolVersion
  )

  def connectionTrace: List[ConnectionType] = any.findFirst(NsiHeaders.NullConnectionTraceElement).map(_.getConnection.asScala.toList) getOrElse Nil

  def withConnectionTrace(trace: List[ConnectionType]): NsiHeaders = copy(any = if (trace.isEmpty) {
    any.remove(NsiHeaders.NullConnectionTraceElement)
  } else {
    any.update(NsiHeaders.gnsFactory.createConnectionTrace(new ConnectionTraceType().withConnection(trace.asJava)))
  })

  def addConnectionTrace(value: String): NsiHeaders = {
    val oldTrace = connectionTrace
    val index = if (oldTrace.isEmpty) 0 else oldTrace.map(_.getIndex).max + 1
    val newTrace = new ConnectionType().withIndex(index).withValue(value) :: oldTrace
    withConnectionTrace(newTrace)
  }

  def pathTrace: Option[PathTraceType] = any.findFirst(NsiHeaders.NullPathTraceElement)

  def withPathTrace(pathTrace: PathTraceType): NsiHeaders = copy(any = any.update(NsiHeaders.pathTraceFactory.createPathTrace(pathTrace)))
}

trait NsiOperation
object NsiOperation {
  val CONNECTION_ID = new QName(QNAME_NSI_TYPES.getNamespaceURI, "connectionId")

  val START_TIME = new QName(QNAME_NSI_TYPES.getNamespaceURI, "startTime")
  val END_TIME = new QName(QNAME_NSI_TYPES.getNamespaceURI, "endTime")

  val CAPACITY = new QName(QNAME_NSI_POINT_TO_POINT.getNamespaceURI, "capacity")

  val CURRENT_TIME = new QName("currentTime")
}

sealed trait NsiMessage[+T <: NsiOperation] {
  def headers: NsiHeaders
  def body: T
  def correlationId: CorrelationId = headers.correlationId
}

final case class NsiProviderMessage[+T <: NsiOperation](headers: NsiHeaders, body: T) extends NsiMessage[T] {
  def ack(acknowledgement: NsiAcknowledgement = GenericAck()): NsiProviderMessage[NsiAcknowledgement] = ack(headers, acknowledgement)

  def ackWithCorrectedProviderNsa(providerNsa: String, acknowledgement: NsiAcknowledgement = GenericAck()): NsiProviderMessage[NsiAcknowledgement] =
    ack(headers.copy(providerNSA = providerNsa), acknowledgement)

  private def ack(ackHeaders: NsiHeaders, ack: NsiAcknowledgement) =
    NsiProviderMessage(ackHeaders.forSyncAck.copy(protocolVersion = NsiHeaders.ProviderProtocolVersion), ack)

  def reply(reply: NsiRequesterOperation) = NsiRequesterMessage(headers.forAsyncReply, reply)
}

final case class NsiRequesterMessage[+T <: NsiOperation](headers: NsiHeaders, body: T) extends NsiMessage[T] {
  def ack(acknowledgement: NsiAcknowledgement = GenericAck()): NsiRequesterMessage[NsiAcknowledgement] = ack(headers, acknowledgement)

  def ackWithCorrectedRequesterNsa(requesterNsa: String, acknowledgement: NsiAcknowledgement = GenericAck()): NsiRequesterMessage[NsiAcknowledgement] =
    ack(headers.copy(requesterNSA = requesterNsa), acknowledgement)

  private def ack(ackHeaders: NsiHeaders, ack: NsiAcknowledgement) =
    NsiRequesterMessage(ackHeaders.forSyncAck.copy(protocolVersion = NsiHeaders.RequesterProtocolVersion), ack)
}

final case class NsiError(id: String, description: String, text: String, variables: Seq[(QName, String)]) {
  override def toString = s"$id: $description: $text"

  def toServiceException(nsaId: String, args: (QName, String)*) = {
    val variables = (this.variables ++ args) map { case (t, v) => new TypeValuePairType().withNamespace(t.getNamespaceURI).withType(t.getLocalPart).withValue(v) }

    new ServiceExceptionType()
      .withErrorId(id)
      .withText(text)
      .withNsaId(nsaId)
      .withVariables(new VariablesType().withVariable(variables.asJava))
  }

  def withText(text: String, variables: (QName, String)*): NsiError = copy(text = text, variables = variables)
  def withVariables(variables: (QName, String)*): NsiError = copy(variables = variables)
}

object NsiError {
  def apply(id: String, description: String, text: String) = new NsiError(id, description, text, Seq.empty)

  val GenericMessagePayloadError = NsiError("00100", "GENERIC_MESSAGE_PAYLOAD_ERROR", "Illegal message payload")
  @deprecated(message = "use GenericMessagePayloadError", since = "NSI CS 2.1") val PayloadError = GenericMessagePayloadError
  val MissingParameter = NsiError("00101", "MISSING_PARAMETER", "Invalid or missing parameter")
  val UnsupportedParameter = NsiError("00102", "UNSUPPORTED_PARAMETER", "Parameter provided contains an unsupported value that MUST be processed")
  val NotImplemented = NsiError("00103", "NOT_IMPLEMENTED", "Requested feature has not been implemented")
  val VersionNotSupported = NsiError("00104", "VERSION_NOT_SUPPORTED", "The protocol version requested is not supported")

  val GenericConnectionError = NsiError("00200", "GENERIC_CONNECTION_ERROR", "A connection error has occurred")
  @deprecated(message = "use GenericConnectionError", since = "NSI CS 2.1") val ConnectionError = GenericConnectionError
  val InvalidTransition = NsiError("00201", "INVALID_TRANSITION", "Connection state machine is in invalid state for received message")
  val ReservationNonExistent = NsiError("00203", "RESERVATION_NONEXISTENT", "Schedule does not exist for connectionId")
  @deprecated(message = "use ReservationNonExistent", since = "NSI CS 2.1") val ConnectionNonExistent = ReservationNonExistent

  val SecurityError = NsiError("00300", "GENERIC_SECURITY_ERROR", "A security error has occurred")
  val Unauthorized = NsiError("00302", "UNAUTHORIZED", "Insufficient authorization to perform requested operation")
  @deprecated(message = "use Unauthorized", since = "NSI CS 2.1") val AuthenticationFailure = Unauthorized

  val TopologyError = NsiError("00400", "GENERIC_METADATA_ERROR", "A topology error has occurred")
  @deprecated(message = "removed", since = "NSI CS 2.1") val StpResolutionError = NsiError("00402", "STP_RESOLUTION_ERROR", "Could not resolve STP to a managing NSA")
  @deprecated(message = "use NoServicePlanePathFound", since = "NSI CS 2.1") val NoPathFound = NsiError("00403", "NO_PATH_FOUND", "Path computation failed to resolve route for reservation")
  @deprecated(message = "removed", since = "NSI CS 2.1") val VlanIdInterchangeNotSupported = NsiError("00404", "VLANID_INTERCHANGE_NOT_SUPPORTED", "VlanId interchange not supported for requested path")
  val DomainLookupError = NsiError("00405", "DOMAIN_LOOKUP_ERROR", "Unknown network for requested resource")
  val NsaLookupError = NsiError("00406", "NSA_LOOKUP_ERROR", "Cannot map networkId to service interface")
  val NoServicePlanePathFound = NsiError("00407", "NO_SERVICEPLANE_PATH_FOUND", "No service plane path for selected connection segments")

  val GenericInternalError = NsiError("00500", "GENERIC_INTERNAL_ERROR", "An internal error has caused a message processing failure")
  @deprecated(message = "use GenericInternalError", since = "NSI CS 2.1") val InternalError = GenericInternalError
  val InternalNrmError = NsiError("00501", "INTERNAL_NRM_ERROR", "An internal NRM error has caused a message processing failure")

  val ChildSegmentError = NsiError("00502", "CHILD_SEGMENT_ERROR", "Child connection segment error is present")
  val MessageDeliveryError = NsiError("00503", "MESSAGE_DELIVERY_ERROR", "Failed message delivery to peer NSA")

  val GenericResourceUnavailable = NsiError("00600", "GENERIC_RESOURCE_UNAVAILABLE", "A requested resource is not available")
  @deprecated(message = "use GenericResourceUnavailable", since = "NSI CS 2.1") val ResourceUnavailable = GenericResourceUnavailable

  val GenericServiceError = NsiError("00700", "GENERIC_SERVICE_ERROR", "Reserved for service specific errors as defined by serviceType and the corresponding service definition")

  // NSI-CS point-to-point service-specific errors
  val UnknownStp = NsiError("00701", "UNKNOWN_STP", "Could not find STP in topology database")
  val LabelSwappingNotSupported = NsiError("00703", "LABEL_SWAPPING_NOT_SUPPORTED", "Label swapping is not supported for requested path")
  val StpUnavailable = NsiError("00704", "STP_UNAVAILABLE", "Specified STP already in use")
  val CAPACITY_UNAVAILABLE = NsiError("00705", "CAPACITY_UNAVAILABLE", "Insufficient capacity available for reservation")
  def CapacityUnavailable(capacity: Long) = CAPACITY_UNAVAILABLE.withVariables(NsiOperation.CAPACITY -> capacity.toString)

  val DirectionalityMismatch = NsiError("00706", "DIRECTIONALITY_MISMATCH", "Directionality of specified STP does not match requested directionality")
  val InvalidEroMember = NsiError("00707", "INVALID_ERO_MEMBER", "Invalid ERO member")
  val UnknownLabelType = NsiError("00708", "UNKNOWN_LABEL_TYPE", "Specified STP contains an unknown label type")
  val InvalidLabelFormat = NsiError("00709", "InvalidLabelFormat", "Specified STP contains an invalid label")
  val NoTransportPlanePathFound = NsiError("00710", "NO_TRANSPORTPLANE_PATH_FOUND", "Path computation failed to resolve route for reservation")

  val GenericRmError = NsiError("00800", "GENERIC_RM_ERROR", "An internal (N)RM error has caused a message processing failure")

}
