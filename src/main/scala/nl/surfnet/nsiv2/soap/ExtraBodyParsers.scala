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

import javax.xml.soap.SOAPConstants

import nl.surfnet.nsiv2._
import nl.surfnet.nsiv2.messages._
import nl.surfnet.nsiv2.utils._

import org.w3c.dom.Document
import play.api.Logger
import play.api.http.{ContentTypeOf, Writeable}
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.iteratee.Input.Empty
import play.api.libs.iteratee._
import play.api.mvc.BodyParsers.parse.when
import play.api.mvc._

import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.{Failure, Success}

import soap.NsiSoapConversions._

object ExtraBodyParsers {

  type NsiRequesterAction = NsiRequesterMessage[NsiRequesterOperation] => Future[NsiRequesterMessage[NsiAcknowledgement]]
  type NsiProviderAction = NsiProviderMessage[NsiProviderOperation] => Future[NsiProviderMessage[NsiAcknowledgement]]

  implicit val NsiMessageContentType: ContentTypeOf[NsiMessage[_]] = ContentTypeOf(Some(SOAPConstants.SOAP_1_1_CONTENT_TYPE))

  implicit def NsiMessageWriteable[T <: NsiMessage[_]](implicit conversion: Conversion[T, Document]): Writeable[T] = Writeable { message =>
    conversion.andThen(NsiXmlDocumentConversion)(message).toEither.fold({ error =>
      // Exceptions from writeable are swallowed by Play, so log these here.
      Logger.error(s"error while writing NsiMessage to response $error", error)
      throw new java.io.IOException(error)
    }, bytes => bytes)
  }

  def NsiProviderEndPoint(providerNsa: String)(action: NsiProviderAction): Action[NsiProviderMessage[NsiProviderOperation]] =
    NsiEndPoint(nsiProviderOperation)(validateProviderNsa(providerNsa, action))(NsiProviderMessageToDocument[NsiAcknowledgement](None))

  def NsiRequesterEndPoint(requesterNsa: String)(action: NsiRequesterAction): Action[NsiRequesterMessage[NsiRequesterOperation]] =
    NsiEndPoint(nsiRequesterOperation)(validateRequesterNsa(requesterNsa, action))(NsiRequesterMessageToDocument[NsiAcknowledgement](None))

  private def validateProviderNsa(providerNsa: String, action: NsiProviderAction) : NsiProviderAction = { message =>
    if (message.headers.providerNSA == providerNsa) action(message)
    else {
      Logger.info(s"The providerNSA '${message.headers.providerNSA}' does not match the expected providerNSA '$providerNsa'")
      val serviceException = ServiceException(NsiError.UnsupportedParameter.toServiceException(providerNsa, NsiHeaders.PROVIDER_NSA -> message.headers.providerNSA))
      val response = message ackWithCorrectedProviderNsa (providerNsa, serviceException)
      Future.successful(response)
    }
  }

  private def validateRequesterNsa(requesterNsa: String, action: NsiRequesterAction): NsiRequesterAction = { message =>
    if (message.headers.requesterNSA == requesterNsa) action(message)
    else {
      Logger.info(s"The requesterNSA '${message.headers.requesterNSA}' does not match the expected requesterNSA '$requesterNsa'")
      val serviceException = ServiceException(NsiError.UnsupportedParameter.toServiceException(requesterNsa, NsiHeaders.REQUESTER_NSA -> message.headers.requesterNSA))
      val response = message ackWithCorrectedRequesterNsa (requesterNsa, serviceException)
      Future.successful(response)
    }
  }

  def NsiEndPoint[M <: NsiOperation, T[_ <: NsiOperation] <: NsiMessage[_]](parser: BodyParser[T[M]])(action: T[M] => Future[T[NsiAcknowledgement]])(implicit conversion: Conversion[T[NsiAcknowledgement], Document]) = Action.async(parser) { request =>
    action(request.body).map { ack =>
      Logger.debug(s"Ack ${request.remoteAddress} with ${Conversion[T[NsiAcknowledgement], String].apply(ack)}")
      ack
    }.map(Results.Ok(_))
  }

  def soap[T](parser: Conversion[T, Array[Byte]], maxLength: Int = BodyParsers.parse.DEFAULT_MAX_TEXT_LENGTH): BodyParser[T] = when(
    predicate = _.contentType.exists(_ == SOAPConstants.SOAP_1_1_CONTENT_TYPE),
    parser = tolerantSoap(parser, maxLength),
    badResult = _ => Future.successful(Results.UnsupportedMediaType("Expecting Content-Type " + SOAPConstants.SOAP_1_1_CONTENT_TYPE)))

  def tolerantSoap[T](parser: Conversion[T, Array[Byte]], maxLength: Int): BodyParser[T] = BodyParser("SOAP, maxLength=" + maxLength) { request =>
    Traversable.takeUpTo[Array[Byte]](maxLength)
      .apply(
        Iteratee.consume[Array[Byte]]().map { bytes =>
          Logger.debug(s"received SOAP message ${request.uri} from ${request.remoteAddress} with content-type ${request.contentType} ${new String(bytes, "UTF-8")}")
          bytes
        }.map { bytes =>
          parser.invert.apply(bytes)
        }.map { parsed =>
          Logger.debug(s"SOAP message parse result ${parsed}")
          parsed
        }).flatMap(Iteratee.eofOrElse(Results.EntityTooLarge))
      .flatMap {
        case Left(b) =>
          Done(Left(b), Empty)
        case Right(it) =>
          it.flatMap {
            case Failure(error) =>
              Logger.warn(s"SOAP parsing failed ${request.uri} from ${request.remoteAddress} with content-type ${request.contentType}: $error")
              Done(Left(Results.InternalServerError(
                <S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/">
                  <S:Body>
                    <S:Fault>
                      <faultcode>S:Client</faultcode>
                      <faultstring>Error parsing SOAP request: { error }</faultstring>
                    </S:Fault>
                  </S:Body>
                </S:Envelope>).as(SOAPConstants.SOAP_1_1_CONTENT_TYPE)), Empty)
            case Success(xml) =>
              Done(Right(xml), Empty)
          }
      }
  }

  private[soap] def nsiProviderOperation = nsiBodyParser(NsiProviderMessageToDocument[NsiProviderOperation](None))

  private[soap] def nsiRequesterOperation = nsiBodyParser(NsiRequesterMessageToDocument[NsiRequesterOperation](None))

  private def nsiBodyParser[T <: NsiMessage[_]](implicit conversion: Conversion[T, Document]): BodyParser[T] = soap(NsiXmlDocumentConversion).flatMap { soapMessage =>
    BodyParser { requestHeader =>
      val parsedMessage = conversion.invert(soapMessage)

      Logger.debug(s"Received (${requestHeader.uri}): $parsedMessage")

      Done(parsedMessage.toEither.left.map { error =>
        Logger.warn(s"Failed to parse $soapMessage with $error on ${requestHeader.uri}", error)
        Results.InternalServerError(
          <S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/">
            <S:Body>
              <S:Fault>
                <faultcode>S:Client</faultcode>
                <faultstring>Error parsing NSI message in SOAP request: { error }</faultstring>
              </S:Fault>
            </S:Body>
          </S:Envelope>).as(SOAPConstants.SOAP_1_1_CONTENT_TYPE)
      })
    }
  }
}
