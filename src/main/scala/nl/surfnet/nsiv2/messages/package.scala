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

import java.net.URI
import java.util.function.Predicate
import javax.xml.datatype.XMLGregorianCalendar
import nl.surfnet.nsiv2.utils.*
import org.ogf.schemas.nsi._2013._12.connection.types.QuerySummaryResultCriteriaType
import org.ogf.schemas.nsi._2013._12.connection.types.ReservationConfirmCriteriaType
import org.ogf.schemas.nsi._2013._12.connection.types.ReservationRequestCriteriaType
import org.ogf.schemas.nsi._2013._12.connection.types.ScheduleType
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.services.types.TypeValueType
import play.api.libs.functional.syntax.*
import play.api.libs.json.*
import scala.jdk.CollectionConverters.*
import scala.util.Try
import jakarta.xml.bind.JAXBElement
import scala.reflect.ClassTag
import java.time.Instant
import javax.xml.namespace.QName

package object messages {
  type RequesterNsa = String
  type ConnectionId = String
  type GlobalReservationId = URI

  private[messages] val PointToPointObjectFactory =
    new org.ogf.schemas.nsi._2013._12.services.point2point.ObjectFactory()

  val NSI_HEADERS_OBJECT_FACTORY =
    new org.ogf.schemas.nsi._2013._12.framework.headers.ObjectFactory()
  val QNAME_NSI_POINT_TO_POINT: QName = PointToPointObjectFactory.createP2Ps(null).getName()
  val QNAME_NSI_P2PS_CAPACITY: QName = PointToPointObjectFactory.createCapacity(null).getName()
  val QNAME_NSI_HEADERS: QName = NSI_HEADERS_OBJECT_FACTORY.createNsiHeader(null).getName()
  val QNAME_NSI_TYPES: QName = (new org.ogf.schemas.nsi._2013._12.connection.types.ObjectFactory())
    .createAcknowledgment(null)
    .getName()

  private val NULL_P2PS_ELEMENT = PointToPointObjectFactory.createP2Ps(null)
  private val NULL_CAPACITY_P2PS_ELEMENT = PointToPointObjectFactory.createCapacity(null)
  private val NULL_PARAMETER_P2PS_ELEMENT = PointToPointObjectFactory.createParameter(null)

  def valueFormat[T](message: String)(parse: String => Option[T], print: T => String): Format[T] =
    new Format[T] {
      override def reads(json: JsValue): JsResult[T] = json match {
        case JsString(s) =>
          parse(s) match {
            case Some(t) => JsSuccess(t)
            case None    => JsError(Seq(JsPath() -> Seq(JsonValidationError(message, s))))
          }
        case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.jsstring"))))
      }
      override def writes(t: T): JsValue = JsString(print(t))
    }

  def unaryCaseClassFormat[A: Format, B](
      fieldName: String
  )(apply: A => B, unapply: B => A): OFormat[B] =
    (__ \ fieldName).format[A].inmap(apply, unapply)

  implicit val JavaTimeInstantFormat: Format[java.time.Instant] = Format(
    implicitly[Reads[Long]].map(java.time.Instant.ofEpochMilli),
    Writes(d => JsNumber(d.toEpochMilli))
  )
  implicit val UriFormat: Format[URI] = valueFormat("error.expected.uri")(
    parse = s => Try(URI.create(s)).toOption,
    print = _.toASCIIString
  )
  implicit val CorrelationIdFormat: Format[CorrelationId] = valueFormat(
    "error.expected.correlationId"
  )(parse = CorrelationId.fromString, print = _.toString)

  implicit class XmlGregorianCalendarOps(cal: XMLGregorianCalendar) {
    def toInstant: Instant = {
      cal.toGregorianCalendar.toZonedDateTime.toInstant
    }
  }

  implicit class HasXmlAnyOps[A: HasXmlAny](a: A) {
    def any: XmlAny = HasXmlAny[A].any(a)

    def findAny[T: ClassTag](nullElement: JAXBElement[T]): List[T] =
      HasXmlAny[A].findAny(a, nullElement)
    def findFirstAny[T: ClassTag](nullElement: JAXBElement[T]): Option[T] =
      HasXmlAny[A].findFirstAny(a, nullElement)
    def removeAny(nullElement: JAXBElement[?]): Boolean = HasXmlAny[A].removeAny(a, nullElement)
    def updateAny(element: JAXBElement[?]): Unit = HasXmlAny[A].updateAny(a, element)
  }

  implicit class XmlPointToPointServiceOps[A: HasXmlAny](a: A) {
    def withPointToPointService(service: P2PServiceBaseType): A = {
      HasXmlAny[A].updateAny(a, PointToPointObjectFactory.createP2Ps(service))
      a
    }

    def pointToPointService: Option[P2PServiceBaseType] =
      HasXmlAny[A].findFirstAny(a, NULL_P2PS_ELEMENT)
  }

  final val PROTECTION_PARAMETER_TYPE = "protection"
  final val PATH_COMPUTATION_ALGORITHM_PARAMETER_TYPE = "pathComputationAlgorithm"

  implicit class P2PServiceBaseTypeOps(service: P2PServiceBaseType) {
    object parameters {
      def apply(`type`: String): Option[String] = {
        service.getParameter.asScala
          .find { _.getType == `type` }
          .map { _.getValue }
      }

      def update(`type`: String, value: Option[String]): Unit = {
        service.getParameter.removeIf(new Predicate[TypeValueType] {
          def test(v: TypeValueType) = v.getType == `type`
        })
        value.foreach { value =>
          service.getParameter.add(new TypeValueType().withType(`type`).withValue(value))
        }
      }
    }

    def protectionType: Option[ProtectionType] =
      parameters(PROTECTION_PARAMETER_TYPE).flatMap(ProtectionType.fromString)

    def protectionType_=(protection: Option[ProtectionType]): Unit = {
      parameters(PROTECTION_PARAMETER_TYPE) = protection.map(_.toString)
    }

    def sourceStp: Stp = Stp.fromString(service.getSourceSTP).getOrElse {
      throw new IllegalArgumentException(s"invalid source STP ${service.getSourceSTP}")
    }

    def destStp: Stp = Stp.fromString(service.getDestSTP).getOrElse {
      throw new IllegalArgumentException(s"invalid destination STP ${service.getDestSTP}")
    }
  }

  implicit class ReservationRequestCriteriaTypeOps(
      requestCriteria: ReservationRequestCriteriaType
  ) {
    def toModifiedConfirmCriteria(
        previouslyCommittedCriteria: ReservationConfirmCriteriaType
    ): Try[ReservationConfirmCriteriaType] = for {
      committedP2P <- previouslyCommittedCriteria.pointToPointService.toTry(
        s"point2point service is missing from committed criteria"
      )
    } yield {
      val confirmP2P = committedP2P.shallowCopy

      requestCriteria.modifiedCapacity.foreach(confirmP2P.setCapacity)
      requestCriteria.modifiedParameters.foreach { parameter =>
        confirmP2P.parameters(parameter.getType) = Some(parameter.getValue)
      }

      val schedule =
        Option(requestCriteria.getSchedule) getOrElse previouslyCommittedCriteria.getSchedule
      schedule.withStartTime(
        schedule.getStartTime orElse2 previouslyCommittedCriteria.getSchedule.startTime.map2(
          _.toXMLGregorianCalendar()
        )
      )

      val confirmCriteria = new ReservationConfirmCriteriaType()
        .withAny(previouslyCommittedCriteria.getAny)
        .withServiceType(previouslyCommittedCriteria.getServiceType)
        .withSchedule(schedule)
        .withVersion(
          if requestCriteria.getVersion eq null then previouslyCommittedCriteria.getVersion + 1
          else requestCriteria.getVersion
        )
      confirmCriteria.withPointToPointService(confirmP2P)
      confirmCriteria.getOtherAttributes.putAll(previouslyCommittedCriteria.getOtherAttributes)
      confirmCriteria
    }

    def toInitialConfirmCriteria(
        fullySpecifiedSource: String,
        fullySpecifiedDest: String
    ): Try[ReservationConfirmCriteriaType] =
      toModifiedConfirmCriteria(
        new ReservationConfirmCriteriaType()
          .withAny(requestCriteria.getAny)
          .withSchedule(new ScheduleType())
          .withServiceType(requestCriteria.getServiceType)
          .withPointToPointService(
            requestCriteria.pointToPointService.get.shallowCopy
              .withSourceSTP(fullySpecifiedSource)
              .withDestSTP(fullySpecifiedDest)
          )
          .withVersion(0)
      )

    /** Schedule is optional. */
    def schedule: Option[ScheduleType] = Option(requestCriteria.getSchedule)

    def version: Option[Int] =
      if requestCriteria.getVersion eq null then None else Some(requestCriteria.getVersion.intValue)

    def modifiedCapacity: Option[Long] = requestCriteria
      .findFirstAny(NULL_CAPACITY_P2PS_ELEMENT)
      .map(Long2long)
      .orElse(
        // Look in P2PServiceBaseType for backwards compatibility with replayed messages
        requestCriteria.pointToPointService.map(_.getCapacity)
      )
    def withModifiedCapacity(capacity: Long): ReservationRequestCriteriaType = {
      requestCriteria.updateAny(PointToPointObjectFactory.createCapacity(capacity))
      requestCriteria
    }

    def modifiedParameters: Seq[TypeValueType] =
      requestCriteria.findAny(NULL_PARAMETER_P2PS_ELEMENT)
    def withModifiedParameters(parameters: TypeValueType*): Boolean = {
      requestCriteria.removeAny(NULL_PARAMETER_P2PS_ELEMENT)
      requestCriteria
        .getAny()
        .addAll(parameters.map(PointToPointObjectFactory.createParameter).asJava)
    }
  }

  implicit class ReservationConfirmCriteriaTypeOps(criteria: ReservationConfirmCriteriaType) {

    /** Schedule is required. */
    def schedule: ScheduleType = criteria.getSchedule

    def version: Int = criteria.getVersion
  }

  implicit class QuerySummaryResultCriteriaTypeOps(criteria: QuerySummaryResultCriteriaType) {

    /** Schedule is required. */
    def schedule: ScheduleType = criteria.getSchedule
  }

  implicit class ShallowCopyOps[A: ShallowCopyable](a: A) {
    def shallowCopy: A = ShallowCopyable[A].shallowCopy(a)
  }

  implicit val P2PServiceBaseTypeShallowCopyable: ShallowCopyable[P2PServiceBaseType] =
    ShallowCopyable.build { a =>
      new P2PServiceBaseType()
        .withAny(a.getAny)
        .withCapacity(a.getCapacity)
        .withDestSTP(a.getDestSTP)
        .withDirectionality(a.getDirectionality)
        .withEro(a.getEro)
        .withParameter(a.getParameter)
        .withSourceSTP(a.getSourceSTP)
        .withSymmetricPath(a.isSymmetricPath)
    }

  implicit val ScheduleTypeShallowCopyable: ShallowCopyable[ScheduleType] = ShallowCopyable.build {
    a =>
      new ScheduleType().withStartTime(a.getStartTime).withEndTime(a.getEndTime)
  }

  private[messages] implicit class RichString(str: String) {
    def uncapitalize: String = str.take(1).toLowerCase + str.drop(1)
  }
}
