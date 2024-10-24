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
package nl.surfnet.nsiv2.soap

import org.apache.pekko.util.ByteString
import jakarta.xml.bind.JAXBContext
import jakarta.xml.bind.JAXBElement
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.net.URI
import javax.xml.XMLConstants
import javax.xml.datatype.XMLGregorianCalendar
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMResult
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.Schema
import nl.surfnet.nsiv2.messages.{given, *}
import nl.surfnet.nsiv2.utils.*
import org.ogf.schemas.nsi._2013._12.connection.types.*
import org.ogf.schemas.nsi._2013._12.framework.headers.CommonHeaderType
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType
import org.w3c.dom.{Document, Element}
import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.SAXParseException
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.xml.parsing.NoBindingFactoryAdapter
import javax.xml.transform.sax.SAXResult

object NsiSoapConversions:
  given ByteArrayToString: Conversion[ByteString, String] =
    Conversion.build[ByteString, String] { bytes =>
      Try(bytes.utf8String)
    } { string =>
      Try(ByteString(string, "UTF-8"))
    }

  val NsiXmlDocumentConversion: Conversion[Document, ByteString] = XmlDocumentConversion(
    new nl.surfnet.bod.nsi.Validation(nl.surfnet.bod.nsi.Validation.NSI_SCHEMAS).getSchema()
  )

  def documentToScalaXml(document: Document): scala.xml.Node =
    val source = new DOMSource(document)
    val adapter = new NoBindingFactoryAdapter:
      // `endDocument` in the Scala 3 XML library is broken since it requires a call to `loadDocument`
      // first to initialize the XML reader. We don't need any of these features so do nothing here.
      override def endDocument(): Unit =
        ()
    val saxResult = new SAXResult(adapter)
    val transformerFactory = javax.xml.transform.TransformerFactory.newInstance()
    val transformer = transformerFactory.newTransformer()
    transformer.transform(source, saxResult)
    adapter.rootElem

  given DocumentToString: Conversion[Document, String] =
    NsiXmlDocumentConversion.andThen(ByteArrayToString)

  given NsiToString[T](using conversion: Conversion[T, Document]): Conversion[T, String] =
    conversion.andThen(NsiXmlDocumentConversion).andThen(ByteArrayToString)

  private val typesFactory = new org.ogf.schemas.nsi._2013._12.connection.types.ObjectFactory()
  private val headersFactory = new org.ogf.schemas.nsi._2013._12.framework.headers.ObjectFactory()
  private val pointToPointServiceFactory =
    new org.ogf.schemas.nsi._2013._12.services.point2point.ObjectFactory()
  private val gnsFactory = new net.nordu.namespaces._2013._12.gnsbod.ObjectFactory()
  private val samlFactory = new oasis.names.tc.saml._2_0.assertion.ObjectFactory()
  private val pathTraceFactory =
    new org.ogf.schemas.nsi._2015._04.connection.pathtrace.ObjectFactory()
  private val SchemaPackages = Seq[AnyRef](
    typesFactory,
    headersFactory,
    pointToPointServiceFactory,
    gnsFactory,
    samlFactory,
    pathTraceFactory
  ).map(_.getClass().getPackage().getName())

  def NsiProviderMessageToDocument[T <: NsiOperation](defaultHeaders: Option[NsiHeaders])(using
      bodyConversion: Conversion[T, Element]
  ): Conversion[NsiProviderMessage[T], Document] = (Conversion
    .build[NsiProviderMessage[T], (Option[NsiHeaders], T)] { message =>
      Success((Some(message.headers), message.body))
    } { case (headers, body) =>
      headers
        .orElse(defaultHeaders)
        .map(headers => Success(NsiProviderMessage(headers, body)))
        .getOrElse(Failure(ErrorMessage("missing NSI headers")))
    })
    .andThen(NsiHeadersAndBodyToDocument[T](bodyConversion))

  def NsiRequesterMessageToDocument[T <: NsiOperation](defaultHeaders: Option[NsiHeaders])(using
      bodyConversion: Conversion[T, Element]
  ): Conversion[NsiRequesterMessage[T], Document] = (Conversion
    .build[NsiRequesterMessage[T], (Option[NsiHeaders], T)] { message =>
      Success((Some(message.headers), message.body))
    } { case (headers, body) =>
      headers
        .orElse(defaultHeaders)
        .map(headers => Success(NsiRequesterMessage(headers, body)))
        .getOrElse(Failure(ErrorMessage("missing NSI headers")))
    })
    .andThen(NsiHeadersAndBodyToDocument[T](bodyConversion))

  private def marshal(jaxb: JAXBElement[?]): Try[Element] = Try {
    val result = new DOMResult()
    jaxbContext.createMarshaller().marshal(jaxb, result)
    result.getNode().asInstanceOf[Document].getDocumentElement()
  }

  given Conversion[NsiAcknowledgement, Element] =
    Conversion.build[NsiAcknowledgement, Element] {
      case GenericAck() =>
        marshal(typesFactory.createAcknowledgment(new GenericAcknowledgmentType()))
      case ReserveResponse(connectionId) =>
        marshal(
          typesFactory.createReserveResponse(
            new ReserveResponseType().withConnectionId(connectionId)
          )
        )
      case QuerySummarySyncConfirmed(reservations, lastModified) =>
        marshal(
          typesFactory.createQuerySummarySyncConfirmed(
            new QuerySummaryConfirmedType()
              .withReservation(reservations.asJava)
              .withLastModified(lastModified.orNull)
          )
        )
      case QueryNotificationSyncConfirmed(notifications) =>
        marshal(
          typesFactory.createQueryNotificationSyncConfirmed(
            new QueryNotificationConfirmedType()
              .withErrorEventOrReserveTimeoutOrDataPlaneStateChange(notifications.asJava)
          )
        )
      case QueryResultSyncConfirmed(results) =>
        marshal(
          typesFactory.createQueryResultSyncConfirmed(
            new QueryResultConfirmedType().withResult(results.asJava)
          )
        )
      case ErrorAck(error) =>
        marshal(typesFactory.createError(error))
      case ServiceException(exception) =>
        // Wrap the service exception in a SOAP Fault element using the Java DOM API.
        marshal(typesFactory.createServiceException(exception)).flatMap { detailBody =>
          Try {
            val doc = createDocument
            val fault = doc.createElementNS(SoapNamespaceUri, "soapenv:Fault").tap(doc.appendChild)
            fault
              .appendChild(doc.createElementNS(null, "faultcode"))
              .appendChild(doc.createTextNode("soapenv:Server")) // FIXME or S:Client?
            fault
              .appendChild(doc.createElementNS(null, "faultstring"))
              .appendChild(doc.createTextNode(exception.getText()))
            fault
              .appendChild(doc.createElementNS(null, "detail"))
              .appendChild(doc.importNode(detailBody, true))
            doc.getDocumentElement()
          }
        }
    } {
      messageFactories(
        Map[String, NsiMessageParser[NsiAcknowledgement]](
          "acknowledgment" -> NsiMessageParser { (_: GenericAcknowledgmentType) =>
            Success(GenericAck())
          },
          "reserveResponse" -> NsiMessageParser { (body: ReserveResponseType) =>
            Success(ReserveResponse(body.getConnectionId()))
          },
          "querySummarySyncConfirmed" -> NsiMessageParser { (body: QuerySummaryConfirmedType) =>
            Success(
              QuerySummarySyncConfirmed(
                body.getReservation().asScala.toVector,
                Option(body.getLastModified)
              )
            )
          },
          "queryNotificationSyncConfirmed" -> NsiMessageParser {
            (body: QueryNotificationConfirmedType) =>
              Success(
                QueryNotificationSyncConfirmed(
                  body.getErrorEventOrReserveTimeoutOrDataPlaneStateChange().asScala.toVector
                )
              )
          },
          "error" -> NsiMessageParser { (body: GenericErrorType) =>
            Success(ErrorAck(body))
          },
          "serviceException" -> NsiMessageParser { (body: ServiceExceptionType) =>
            Success(ServiceException(body))
          }
        )
      )
    }

  given Conversion[NsiProviderOperation, Element] =
    Conversion.build[NsiProviderOperation, Element] { operation =>
      marshal(operation match
        case InitialReserve(body) => typesFactory.createReserve(body)
        case ModifyReserve(body)  => typesFactory.createReserve(body)
        case ReserveCommit(connectionId) =>
          typesFactory.createReserveCommit(new GenericRequestType().withConnectionId(connectionId))
        case ReserveAbort(connectionId) =>
          typesFactory.createReserveAbort(new GenericRequestType().withConnectionId(connectionId))
        case Provision(connectionId) =>
          typesFactory.createProvision(new GenericRequestType().withConnectionId(connectionId))
        case Release(connectionId) =>
          typesFactory.createRelease(new GenericRequestType().withConnectionId(connectionId))
        case Terminate(connectionId) =>
          typesFactory.createTerminate(new GenericRequestType().withConnectionId(connectionId))
        case QuerySummary(ids, ifModifiedSince) =>
          typesFactory.createQuerySummary(
            toQueryType(ids).withIfModifiedSince(ifModifiedSince.orNull)
          )
        case QuerySummarySync(ids, ifModifiedSince) =>
          typesFactory.createQuerySummarySync(
            toQueryType(ids).withIfModifiedSince(ifModifiedSince.orNull)
          )
        case QueryRecursive(ids, ifModifiedSince) =>
          typesFactory.createQueryRecursive(
            toQueryType(ids).withIfModifiedSince(ifModifiedSince.orNull)
          )
        case QueryNotification(connectionId, start, end) =>
          typesFactory.createQueryNotification(
            new QueryNotificationType()
              .withConnectionId(connectionId)
              .withStartNotificationId(if start.isDefined then start.get else null)
              .withEndNotificationId(if end.isDefined then end.get else null)
          )
        case QueryNotificationSync(connectionId, start, end) =>
          typesFactory.createQueryNotificationSync(
            new QueryNotificationType()
              .withConnectionId(connectionId)
              .withStartNotificationId(if start.isDefined then start.get else null)
              .withEndNotificationId(if end.isDefined then end.get else null)
          )
        case QueryResult(connectionId, start, end) =>
          typesFactory.createQueryResult(
            new QueryResultType()
              .withConnectionId(connectionId)
              .withStartResultId(if start.isDefined then start.get else null)
              .withEndResultId(if end.isDefined then end.get else null)
          )
        case QueryResultSync(connectionId, start, end) =>
          typesFactory.createQueryResultSync(
            new QueryResultType()
              .withConnectionId(connectionId)
              .withStartResultId(if start.isDefined then start.get else null)
              .withEndResultId(if end.isDefined then end.get else null)
          )
      )
    } {
      messageFactories(
        Map[String, NsiMessageParser[NsiProviderOperation]](
          "reserve" -> NsiMessageParser { (body: ReserveType) =>
            if body.getConnectionId eq null then
              for _ <- body.getCriteria.pointToPointService
                  .toTry("initial reserve is missing point2point service")
              yield InitialReserve(body)
            else Success(ModifyReserve(body))
          },
          "reserveCommit" -> NsiMessageParser { (body: GenericRequestType) =>
            Success(ReserveCommit(body.getConnectionId()))
          },
          "reserveAbort" -> NsiMessageParser { (body: GenericRequestType) =>
            Success(ReserveAbort(body.getConnectionId()))
          },
          "provision" -> NsiMessageParser { (body: GenericRequestType) =>
            Success(Provision(body.getConnectionId()))
          },
          "release" -> NsiMessageParser { (body: GenericRequestType) =>
            Success(Release(body.getConnectionId()))
          },
          "terminate" -> NsiMessageParser { (body: GenericRequestType) =>
            Success(Terminate(body.getConnectionId()))
          },
          "querySummary" -> NsiMessageParser { (body: QueryType) =>
            Success(QuerySummary(toIds(body), toIfModifiedSince(body)))
          },
          "querySummarySync" -> NsiMessageParser { (body: QueryType) =>
            Success(QuerySummarySync(toIds(body), toIfModifiedSince(body)))
          },
          "queryRecursive" -> NsiMessageParser { (body: QueryType) =>
            Success(QueryRecursive(toIds(body), toIfModifiedSince(body)))
          },
          "queryNotification" -> NsiMessageParser { (body: QueryNotificationType) =>
            Success(
              QueryNotification(
                body.getConnectionId(),
                Option(body.getStartNotificationId()).map(_.toInt),
                Option(body.getEndNotificationId()).map(_.toInt)
              )
            )
          },
          "queryNotificationSync" -> NsiMessageParser { (body: QueryNotificationType) =>
            Success(
              QueryNotificationSync(
                body.getConnectionId(),
                Option(body.getStartNotificationId()).map(_.toInt),
                Option(body.getEndNotificationId()).map(_.toInt)
              )
            )
          },
          "queryResult" -> NsiMessageParser { (body: QueryResultType) =>
            Success(
              QueryResult(
                body.getConnectionId(),
                Option(body.getStartResultId()).map(_.toInt),
                Option(body.getEndResultId()).map(_.toInt)
              )
            )
          },
          "queryResultSync" -> NsiMessageParser { (body: QueryResultType) =>
            Success(
              QueryResultSync(
                body.getConnectionId(),
                Option(body.getStartResultId()).map(_.toInt),
                Option(body.getEndResultId()).map(_.toInt)
              )
            )
          }
        )
      )
    }

  private def toIds(query: QueryType): Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]] =
    if !query.getConnectionId().isEmpty() then Some(Left(query.getConnectionId().asScala.toSeq))
    else if !query.getGlobalReservationId().isEmpty then
      Some(Right(query.getGlobalReservationId().asScala.toSeq.map(URI.create)))
    else None

  private def toIfModifiedSince(query: QueryType): Option[XMLGregorianCalendar] = Option(
    query.getIfModifiedSince
  )

  private def toQueryType(
      ids: Option[Either[Seq[ConnectionId], Seq[GlobalReservationId]]]
  ): QueryType = ids match
    case Some(Left(connectionIds)) => new QueryType().withConnectionId(connectionIds.asJava)
    case Some(Right(globalReservationIds)) =>
      new QueryType().withGlobalReservationId(globalReservationIds.map(_.toASCIIString()).asJava)
    case None => new QueryType()

  given Conversion[NsiRequesterOperation, Element] =
    Conversion.build[NsiRequesterOperation, Element] { operation =>
      marshal(operation match
        case ReserveConfirmed(connectionId, criteria) =>
          typesFactory.createReserveConfirmed(
            new ReserveConfirmedType().withConnectionId(connectionId).withCriteria(criteria)
          )
        case ReserveFailed(failure) => typesFactory.createReserveFailed(failure)
        case ReserveCommitConfirmed(connectionId) =>
          typesFactory.createReserveCommitConfirmed(
            new GenericConfirmedType().withConnectionId(connectionId)
          )
        case ReserveCommitFailed(failure) => typesFactory.createReserveCommitFailed(failure)
        case ReserveAbortConfirmed(connectionId) =>
          typesFactory.createReserveAbortConfirmed(
            new GenericConfirmedType().withConnectionId(connectionId)
          )
        case ReserveTimeout(timeout) => typesFactory.createReserveTimeout(timeout)
        case ProvisionConfirmed(connectionId) =>
          typesFactory.createProvisionConfirmed(
            new GenericConfirmedType().withConnectionId(connectionId)
          )
        case ReleaseConfirmed(connectionId) =>
          typesFactory.createReleaseConfirmed(
            new GenericConfirmedType().withConnectionId(connectionId)
          )
        case TerminateConfirmed(connectionId) =>
          typesFactory.createTerminateConfirmed(
            new GenericConfirmedType().withConnectionId(connectionId)
          )
        case QuerySummaryConfirmed(reservations, lastModified) =>
          typesFactory.createQuerySummaryConfirmed(
            new QuerySummaryConfirmedType()
              .withReservation(reservations.asJava)
              .withLastModified(lastModified.orNull)
          )
        case QueryRecursiveConfirmed(reservations) =>
          typesFactory.createQueryRecursiveConfirmed(
            new QueryRecursiveConfirmedType().withReservation(reservations.asJava)
          )
        case QueryNotificationConfirmed(notifications) =>
          typesFactory.createQueryNotificationConfirmed(
            new QueryNotificationConfirmedType()
              .withErrorEventOrReserveTimeoutOrDataPlaneStateChange(notifications.asJava)
          )
        case QueryResultConfirmed(results) =>
          typesFactory.createQueryResultConfirmed(
            new QueryResultConfirmedType().withResult(results.asJava)
          )
        case ErrorReply(error) => typesFactory.createError(error)
        case DataPlaneStateChange(notification) =>
          typesFactory.createDataPlaneStateChange(notification)
        case ErrorEvent(error)               => typesFactory.createErrorEvent(error)
        case MessageDeliveryTimeout(timeout) => typesFactory.createMessageDeliveryTimeout(timeout)
      )
    } {
      messageFactories(
        Map[String, NsiMessageParser[NsiRequesterOperation]](
          "reserveConfirmed" -> NsiMessageParser { (body: ReserveConfirmedType) =>
            Success(ReserveConfirmed(body.getConnectionId(), body.getCriteria()))
          },
          "reserveFailed" -> NsiMessageParser { (body: GenericFailedType) =>
            Success(ReserveFailed(body))
          },
          "reserveCommitConfirmed" -> NsiMessageParser { (body: GenericConfirmedType) =>
            Success(ReserveCommitConfirmed(body.getConnectionId))
          },
          "reserveCommitFailed" -> NsiMessageParser { (body: GenericFailedType) =>
            Success(ReserveCommitFailed(body))
          },
          "reserveAbortConfirmed" -> NsiMessageParser { (body: GenericConfirmedType) =>
            Success(ReserveAbortConfirmed(body.getConnectionId))
          },
          "reserveTimeout" -> NsiMessageParser { (body: ReserveTimeoutRequestType) =>
            Success(ReserveTimeout(body))
          },
          "provisionConfirmed" -> NsiMessageParser { (body: GenericConfirmedType) =>
            Success(ProvisionConfirmed(body.getConnectionId))
          },
          "releaseConfirmed" -> NsiMessageParser { (body: GenericConfirmedType) =>
            Success(ReleaseConfirmed(body.getConnectionId))
          },
          "terminateConfirmed" -> NsiMessageParser { (body: GenericConfirmedType) =>
            Success(TerminateConfirmed(body.getConnectionId))
          },
          "querySummaryConfirmed" -> NsiMessageParser { (body: QuerySummaryConfirmedType) =>
            Success(
              QuerySummaryConfirmed(
                body.getReservation().asScala.toVector,
                Option(body.getLastModified)
              )
            )
          },
          "queryRecursiveConfirmed" -> NsiMessageParser { (body: QueryRecursiveConfirmedType) =>
            Success(QueryRecursiveConfirmed(body.getReservation().asScala.toVector))
          },
          "queryNotificationConfirmed" -> NsiMessageParser {
            (body: QueryNotificationConfirmedType) =>
              Success(
                QueryNotificationConfirmed(
                  body.getErrorEventOrReserveTimeoutOrDataPlaneStateChange().asScala.toVector
                )
              )
          },
          "error" -> NsiMessageParser { (body: GenericErrorType) => Success(ErrorReply(body)) },
          "dataPlaneStateChange" -> NsiMessageParser { (body: DataPlaneStateChangeRequestType) =>
            Success(DataPlaneStateChange(body))
          },
          "errorEvent" -> NsiMessageParser { (body: ErrorEventType) => Success(ErrorEvent(body)) },
          "messageDeliveryTimeout" -> NsiMessageParser {
            (body: MessageDeliveryTimeoutRequestType) => Success(MessageDeliveryTimeout(body))
          }
        )
      )
    }

  private given Conversion[NsiHeaders, CommonHeaderType] =
    Conversion.build[NsiHeaders, CommonHeaderType] { headers =>
      Try(
        new CommonHeaderType()
          .withCorrelationId(headers.correlationId.toString)
          .withReplyTo(headers.replyTo.map(_.toASCIIString()).orNull)
          .withProtocolVersion(headers.protocolVersion.toASCIIString())
          .withProviderNSA(headers.providerNSA)
          .withRequesterNSA(headers.requesterNSA)
          .withSessionSecurityAttr(headers.sessionSecurityAttrs.asJava)
          .tap(_.getAny().addAll(headers.any.elements.asJava))
          .tap(_.getOtherAttributes().putAll(headers.otherAttributes.asJava))
      )
    } { header =>
      for
        correlationId <- CorrelationId
          .fromString(header.getCorrelationId())
          .toTry(s"invalid correlation id ${header.getCorrelationId()}")
        replyTo <- Try(Option(header.getReplyTo()).map(URI.create))
        protocolVersion <- Try(URI.create(header.getProtocolVersion()))
      yield NsiHeaders(
        correlationId,
        header.getRequesterNSA(),
        header.getProviderNSA(),
        replyTo,
        protocolVersion,
        header.getSessionSecurityAttr.asScala.toList,
        XmlAny(header.getAny.asScala.toList),
        header.getOtherAttributes.asScala.toMap
      )
    }

  private trait NsiMessageParser[T]:
    def apply(bodyNode: Element): Try[T]
  private object NsiMessageParser:
    def apply[M, T](f: M => Try[T])(using manifest: ClassTag[M]): NsiMessageParser[T] =
      new NsiMessageParser[T]:
        override def apply(bodyNode: Element) =
          val unmarshaller = jaxbContext.createUnmarshaller()
          for
            body <- Try(
              unmarshaller.unmarshal(bodyNode, manifest.runtimeClass.asInstanceOf[Class[M]])
            )
            message <- f(body.getValue())
          yield message

  private def messageFactories[T](
      factories: Map[String, NsiMessageParser[T]]
  )(bodyNode: Element): Try[T] =
    for
      parser <- factories
        .get(bodyNode.getLocalName())
        .toTry(s"unknown body element type '${bodyNode.getLocalName()}'")
      body <- parser(bodyNode)
    yield body

  private val SoapNamespaceUri = "http://schemas.xmlsoap.org/soap/envelope/"
  private val NsiHeadersQName = headersFactory.createNsiHeader(null).getName()
  private val NsiConnectionTypesNamespace =
    typesFactory.createAcknowledgment(null).getName().getNamespaceURI()

  private def XmlDocumentConversion(schema: Schema): Conversion[Document, ByteString] =
    val errorHandler = new DefaultHandler():
      override def error(e: SAXParseException): Nothing = throw e
      override def fatalError(e: SAXParseException): Nothing = throw e

    Conversion.build[Document, ByteString] { document =>
      Try {
        val domSource = new DOMSource(document)

        val validator = schema.newValidator()
        validator.setErrorHandler(errorHandler)
        validator.validate(domSource)

        val transformer = TransformerFactory.newInstance().newTransformer()

        val baos = new ByteArrayOutputStream()
        transformer.transform(domSource, new StreamResult(baos))
        ByteString(baos.toByteArray())
      }
    } { bytes =>
      Try {
        val dbf = DocumentBuilderFactory.newInstance()
        dbf.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
        dbf.setNamespaceAware(true)
        dbf.setIgnoringComments(true)
        dbf.setIgnoringElementContentWhitespace(true)
        dbf.setSchema(schema)

        val db = dbf.newDocumentBuilder()
        db.setErrorHandler(errorHandler)
        db.parse(new ByteArrayInputStream(bytes.toArray))
      }
    }
  end XmlDocumentConversion

  val jaxbContext: JAXBContext = JAXBContext.newInstance(SchemaPackages.mkString(":"))

  private given Conversion[NsiHeaders, Element] =
    Conversion.build[NsiHeaders, Element] { headers =>
      for
        commonHeaderType <- Conversion[NsiHeaders, CommonHeaderType].apply(headers)
        headersElement <- marshal(headersFactory.createNsiHeader(commonHeaderType))
      yield headersElement
    } { headersElement =>
      val unmarshaller = jaxbContext.createUnmarshaller()
      for
        commonHeaderType <- Try(
          unmarshaller.unmarshal(headersElement, classOf[CommonHeaderType]).getValue
        )
        headers <- Conversion[NsiHeaders, CommonHeaderType].invert(commonHeaderType)
      yield headers
    }

  private def NsiJaxbElementToString[T](jaxb: JAXBElement[T]): Conversion[T, String] =
    Conversion.build[T, String] { value =>
      Try {
        val wrapped = new JAXBElement(jaxb.getName(), jaxb.getDeclaredType(), value)
        val marshaller = jaxbContext.createMarshaller()
        val baos = new ByteArrayOutputStream()
        marshaller.marshal(wrapped, baos)
        baos.toString("UTF-8")
      }
    } { s =>
      Try {
        val unmarshaller = jaxbContext.createUnmarshaller()
        val source = new StreamSource(new ByteArrayInputStream(s.getBytes("UTF-8")))
        unmarshaller.unmarshal(source, jaxb.getDeclaredType()).getValue()
      }
    }

  given NsiHeadersToXmlString: Conversion[NsiHeaders, String] =
    Conversion[NsiHeaders, CommonHeaderType] andThen NsiJaxbElementToString(
      headersFactory.createNsiHeader(null)
    )
  given ServiceExceptionTypeToXmlString: Conversion[ServiceExceptionType, String] =
    NsiJaxbElementToString(typesFactory.createServiceException(null))

  private def NsiHeadersAndBodyToDocument[T](bodyConversion: Conversion[T, Element]) =
    Conversion.build[(Option[NsiHeaders], T), Document] { case (headers, body) =>
      for
        headersElementOption <- headers.map(Conversion[NsiHeaders, Element].apply).sequence
        bodyElement <- bodyConversion(body)
        document <- Try {
          val document = createDocument
          val soapEnvelope =
            document.createElementNS(SoapNamespaceUri, "soapenv:Envelope").tap(document.appendChild)
          soapEnvelope.setAttributeNS(
            "http://www.w3.org/2000/xmlns/",
            "xmlns:soapenv",
            SoapNamespaceUri
          )

          val soapHeader =
            soapEnvelope.appendChild(document.createElementNS(SoapNamespaceUri, "soapenv:Header"))
          val soapBody =
            soapEnvelope.appendChild(document.createElementNS(SoapNamespaceUri, "soapenv:Body"))

          headersElementOption.foreach(it => soapHeader.appendChild(document.importNode(it, true)))
          soapBody.appendChild(document.importNode(bodyElement, true))

          document
        }
      yield document
    } { document =>
      for
        soapEnvelope <- Option(document.getDocumentElement).toTry("missing document root")
        header <- parseNsiHeaders(soapEnvelope)
        soapBody <- findSingleChildElement(SoapNamespaceUri, "Body", soapEnvelope)
        soapFaultNode <- findOptionalChildElement(SoapNamespaceUri, "Fault", soapBody)
        body <- soapFaultNode
          .map(n => parseSoapFault(n)(bodyConversion))
          .getOrElse(parseSoapBody(soapBody)(bodyConversion))
      yield (header, body)
    }

  private def parseNsiHeaders(soapEnvelope: Element): Try[Option[NsiHeaders]] = for
    soapHeader <- findOptionalChildElement(SoapNamespaceUri, "Header", soapEnvelope)
    headerNode <- soapHeader
      .map(it =>
        findOptionalChildElement(
          NsiHeadersQName.getNamespaceURI(),
          NsiHeadersQName.getLocalPart(),
          it
        )
      )
      .getOrElse(Success(None))
    header <- headerNode.map(Conversion[NsiHeaders, Element].invert(_)).sequence
  yield header

  private def parseSoapBody[T](
      soapBody: Element
  )(bodyConversion: Conversion[T, Element]): Try[T] = for
    bodyNode <- findSingleChildElement(NsiConnectionTypesNamespace, "*", soapBody)
    body <- bodyConversion.invert(bodyNode)
  yield body

  private val ServiceExceptionTypeName = typesFactory.createServiceException(null).getName()

  private def parseSoapFault[T](
      soapFault: Element
  )(bodyConversion: Conversion[T, Element]): Try[T] = for
    faultString <- findSingleChildElement("", "faultstring", soapFault)
    serviceExceptionNode <- findOptionalChildElement(
      ServiceExceptionTypeName.getNamespaceURI(),
      ServiceExceptionTypeName.getLocalPart(),
      soapFault
    )
    serviceException <- serviceExceptionNode.map(bodyConversion.invert(_)).sequence
    result <- serviceException.toTry(
      s"SOAP fault without ${ServiceExceptionTypeName}. Fault string: ${faultString.getTextContent()}"
    )
  yield result

  private def createDocument: Document = DocumentBuilderFactory
    .newInstance()
    .tap(_.setNamespaceAware(true))
    .newDocumentBuilder()
    .newDocument()

  private def findSingleChildElement(
      namespaceUri: String,
      localName: String,
      parent: Element
  ): Try[Element] =
    findOptionalChildElement(namespaceUri, localName, parent) match
      case Success(None) =>
        Failure(
          ErrorMessage(
            s"missing element '$namespaceUri:$localName' in '${parent.getLocalName}', expected exactly one"
          )
        )
      case Success(Some(x)) => Success(x)
      case Failure(x)       => Failure(x)

  private def findOptionalChildElement(
      namespaceUri: String,
      localName: String,
      parent: Element
  ): Try[Option[Element]] =
    val childNodes = parent.getElementsByTagNameNS(namespaceUri, localName)
    val children = Vector.tabulate(childNodes.getLength)(childNodes.item)
    children.collect { case e: Element =>
      e
    } match
      case Vector(e) => Success(Some(e))
      case Vector()  => Success(None)
      case _ =>
        Failure(
          ErrorMessage(
            s"multiple elements '$namespaceUri:$localName' in '${parent.getLocalName}', expected exactly one"
          )
        )
end NsiSoapConversions
