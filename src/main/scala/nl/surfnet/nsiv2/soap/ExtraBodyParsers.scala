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

import akka.util.ByteString

import jakarta.xml.soap.SOAPConstants

import nl.surfnet.nsiv2._
import nl.surfnet.nsiv2.messages._

import org.w3c.dom.Document
import play.api.Logger
import play.api.http.{ContentTypeOf, Writeable}
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

import soap.NsiSoapConversions._

class ExtraBodyParsers(implicit
    ec: ExecutionContext,
    bodyParsers: PlayBodyParsers,
    actionBuilder: ActionBuilder[Request, AnyContent]
) {
  private val logger = Logger(classOf[ExtraBodyParsers])

  type NsiRequesterAction =
    NsiRequesterMessage[NsiRequesterOperation] => Future[NsiRequesterMessage[NsiAcknowledgement]]
  type NsiProviderAction =
    NsiProviderMessage[NsiProviderOperation] => Future[NsiProviderMessage[NsiAcknowledgement]]

  implicit val NsiMessageContentType: ContentTypeOf[NsiMessage[_]] = ContentTypeOf(
    Some(SOAPConstants.SOAP_1_1_CONTENT_TYPE)
  )

  implicit def NsiMessageWriteable[T <: NsiMessage[_]](implicit
      conversion: Conversion[T, Document]
  ): Writeable[T] = Writeable { message =>
    conversion
      .andThen(NsiXmlDocumentConversion)(message)
      .toEither
      .fold(
        { error =>
          // Exceptions from writeable are swallowed by Play, so log these here.
          logger.error(s"error while writing NsiMessage to response $error", error)
          throw new java.io.IOException(error)
        },
        bytes => bytes
      )
  }

  def NsiProviderEndPoint(
      providerNsa: String
  )(action: NsiProviderAction): Action[NsiProviderMessage[NsiProviderOperation]] =
    NsiEndPoint(nsiProviderOperation)(validateProviderNsa(providerNsa, action))(
      NsiProviderMessageToDocument[NsiAcknowledgement](None)
    )

  def NsiRequesterEndPoint(
      requesterNsa: String
  )(action: NsiRequesterAction): Action[NsiRequesterMessage[NsiRequesterOperation]] =
    NsiEndPoint(nsiRequesterOperation)(validateRequesterNsa(requesterNsa, action))(
      NsiRequesterMessageToDocument[NsiAcknowledgement](None)
    )

  private def validateProviderNsa(
      providerNsa: String,
      action: NsiProviderAction
  ): NsiProviderAction = { message =>
    if (message.headers.providerNSA == providerNsa) action(message)
    else {
      logger.info(
        s"The providerNSA '${message.headers.providerNSA}' does not match the expected providerNSA '$providerNsa'"
      )
      val serviceException = ServiceException(
        NsiError.UnsupportedParameter
          .toServiceException(providerNsa, NsiHeaders.PROVIDER_NSA -> message.headers.providerNSA)
      )
      val response = message.ackWithCorrectedProviderNsa(providerNsa, serviceException)
      Future.successful(response)
    }
  }

  private def validateRequesterNsa(
      requesterNsa: String,
      action: NsiRequesterAction
  ): NsiRequesterAction = { message =>
    if (message.headers.requesterNSA == requesterNsa) action(message)
    else {
      logger.info(
        s"The requesterNSA '${message.headers.requesterNSA}' does not match the expected requesterNSA '$requesterNsa'"
      )
      val serviceException = ServiceException(
        NsiError.UnsupportedParameter.toServiceException(
          requesterNsa,
          NsiHeaders.REQUESTER_NSA -> message.headers.requesterNSA
        )
      )
      val response = message.ackWithCorrectedRequesterNsa(requesterNsa, serviceException)
      Future.successful(response)
    }
  }

  def NsiEndPoint[M <: NsiOperation, T[_ <: NsiOperation] <: NsiMessage[_]](
      parser: BodyParser[T[M]]
  )(action: T[M] => Future[T[NsiAcknowledgement]])(implicit
      conversion: Conversion[T[NsiAcknowledgement], Document]
  ) = actionBuilder.async(parser) { request =>
    action(request.body)
      .map { ack =>
        logger.debug(
          s"Ack ${request.remoteAddress} with ${Conversion[T[NsiAcknowledgement], String].apply(ack)}"
        )
        ack
      }
      .map(Results.Ok(_))
  }

  def soap[T](
      parser: Conversion[T, ByteString],
      maxLength: Long = bodyParsers.DefaultMaxTextLength
  ): BodyParser[T] = bodyParsers.when(
    predicate = _.contentType.exists(_ == SOAPConstants.SOAP_1_1_CONTENT_TYPE),
    parser = tolerantSoap(parser, maxLength),
    badResult = _ =>
      Future.successful(
        Results.UnsupportedMediaType(
          "Expecting Content-Type " + SOAPConstants.SOAP_1_1_CONTENT_TYPE
        )
      )
  )

  def tolerantSoap[T](parser: Conversion[T, ByteString], maxLength: Long): BodyParser[T] =
    BodyParser("SOAP, maxLength=" + maxLength) { request =>
      val xmlParser = bodyParsers
        .byteString(maxLength)
        .map { bytes =>
          logger.debug(
            s"received SOAP message ${request.uri} from ${request.remoteAddress} with content-type ${request.contentType} ${bytes.utf8String}"
          )
          val parsed = parser.invert.apply(bytes)
          logger.debug(s"SOAP message parse result ${parsed}")
          parsed
        }
        .validate {
          case Failure(error) =>
            logger.warn(
              s"SOAP parsing failed ${request.uri} from ${request.remoteAddress} with content-type ${request.contentType}: $error"
            )
            Left(
              Results
                .InternalServerError(
                  <S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/">
              <S:Body>
                <S:Fault>
                  <faultcode>S:Client</faultcode>
                  <faultstring>Error parsing SOAP request: {error}</faultstring>
                </S:Fault>
              </S:Body>
              </S:Envelope>
                )
                .as(SOAPConstants.SOAP_1_1_CONTENT_TYPE)
            )
          case Success(t) =>
            Right(t)
        }
      xmlParser(request)
    }

  private[soap] def nsiProviderOperation = nsiBodyParser(
    NsiProviderMessageToDocument[NsiProviderOperation](None)
  )

  private[soap] def nsiRequesterOperation = nsiBodyParser(
    NsiRequesterMessageToDocument[NsiRequesterOperation](None)
  )

  private def nsiBodyParser[T <: NsiMessage[_]](implicit
      conversion: Conversion[T, Document]
  ): BodyParser[T] = BodyParser { requestHeader =>
    soap(NsiXmlDocumentConversion)(requestHeader).map {
      case Left(result) =>
        Left(result)
      case Right(soapMessage) =>
        val parsedMessage = conversion.invert(soapMessage)

        logger.debug(s"Received (${requestHeader.uri}): $parsedMessage")

        parsedMessage match {
          case Failure(error) =>
            logger.warn(s"Failed to parse $soapMessage with $error on ${requestHeader.uri}", error)
            Left(
              Results
                .InternalServerError(
                  <S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/">
                <S:Body>
                  <S:Fault>
                    <faultcode>S:Client</faultcode>
                    <faultstring>Error parsing NSI message in SOAP request: {error}</faultstring>
                  </S:Fault>
                </S:Body>
              </S:Envelope>
                )
                .as(SOAPConstants.SOAP_1_1_CONTENT_TYPE)
            )

          case Success(body) =>
            Right(body)
        }
    }
  }
}
