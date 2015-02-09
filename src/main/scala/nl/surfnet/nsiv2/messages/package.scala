package nl.surfnet.nsiv2

import java.net.URI
import java.util.function.Predicate
import javax.xml.bind.JAXBElement
import javax.xml.datatype.{ DatatypeFactory, XMLGregorianCalendar }
import javax.xml.namespace.QName
import nl.surfnet.nsiv2.utils._
import org.joda.time.{ DateTime, DateTimeZone }
import org.ogf.schemas.nsi._2013._12.connection.types.QuerySummaryResultCriteriaType
import org.ogf.schemas.nsi._2013._12.connection.types.ReservationConfirmCriteriaType
import org.ogf.schemas.nsi._2013._12.connection.types.ReservationRequestCriteriaType
import org.ogf.schemas.nsi._2013._12.connection.types.ScheduleType
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.ogf.schemas.nsi._2013._12.services.types.TypeValueType
import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scala.collection.JavaConverters._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

package object messages {
  type RequesterNsa = String
  type ConnectionId = String
  type GlobalReservationId = URI

  def valueFormat[T](message: String)(parse: String => Option[T], print: T => String): Format[T] = new Format[T] {
    override def reads(json: JsValue): JsResult[T] = json match {
      case JsString(s) => parse(s) match {
        case Some(t) => JsSuccess(t)
        case None    => JsError(Seq(JsPath() -> Seq(ValidationError(message, s))))
      }
      case _ => JsError(Seq(JsPath() -> Seq(ValidationError("error.expected.jsstring"))))
    }
    override def writes(t: T): JsValue = JsString(print(t))
  }

  def unaryCaseClassFormat[A: Format, B](fieldName: String)(apply: A => B, unapply: B => Option[A]): OFormat[B] = (__ \ fieldName).format[A].inmap(apply, unlift(unapply))

  implicit val JavaTimeInstantFormat: Format[java.time.Instant] = Format(
    implicitly[Reads[Long]].map(java.time.Instant.ofEpochMilli),
    Writes(d => JsNumber(d.toEpochMilli)))
  implicit val UriFormat: Format[URI] = valueFormat("error.expected.uri")(
    parse = s => Try(URI.create(s)).toOption,
    print = _.toASCIIString)
  implicit val CorrelationIdFormat: Format[CorrelationId] = valueFormat("error.expected.correlationId")(
    parse = CorrelationId.fromString,
    print = _.toString)

  private[this] val datatypeFactory = DatatypeFactory.newInstance()
  implicit class DateTimeOps(dt: org.joda.time.ReadableDateTime) {
    def toXmlGregorianCalendar = {
      val timezoneInMinutes = dt.getZone.getOffset(dt.getMillis) / (60 * 1000)
      datatypeFactory.newXMLGregorianCalendar(
        dt.getYear,
        dt.getMonthOfYear,
        dt.getDayOfMonth,
        dt.getHourOfDay,
        dt.getMinuteOfHour,
        dt.getSecondOfMinute,
        dt.getMillisOfSecond,
        timezoneInMinutes)
    }
  }

  implicit class XmlGregorianCalendarOps(dt: XMLGregorianCalendar) {
    def toDateTime = {
      val calendar = dt.toGregorianCalendar
      new DateTime(calendar.getTimeInMillis, DateTimeZone.forTimeZone(dt.toGregorianCalendar.getTimeZone))
    }
  }

  private val PointToPointObjectFactory = new org.ogf.schemas.nsi._2013._12.services.point2point.ObjectFactory()
  private val P2PS_QNAME = PointToPointObjectFactory.createP2Ps(null).getName()

  private object JaxbElement {
    def unapply[A](element: JAXBElement[A]): Option[(QName, A)] = Some((element.getName(), element.getValue()))
  }
  implicit class XmlPointToPointServiceOps[A: HasXmlAny](a: A) {
    def setPointToPointService(service: P2PServiceBaseType): Unit = {
      val element = PointToPointObjectFactory.createP2Ps(service)

      val any = HasXmlAny[A].getAny(a)
      any.removeIf(new Predicate[AnyRef] {
        def test(v: AnyRef) = v match {
          case JaxbElement(P2PS_QNAME, _: P2PServiceBaseType) => true
          case _ => false
        }
      })
      any.add(element)
      ()
    }

    def withPointToPointService(service: P2PServiceBaseType): A = {
      setPointToPointService(service)
      a
    }

    def getPointToPointService(): Option[P2PServiceBaseType] = HasXmlAny[A].getAny(a).asScala.collectFirst {
      case JaxbElement(P2PS_QNAME, p2ps: P2PServiceBaseType) => p2ps
    }
  }

  final val PROTECTION_PARAMETER_TYPE = "protection"

  implicit class P2PServiceBaseTypeOps(service: P2PServiceBaseType) {
    def protectionType_=(protection: Option[ProtectionType]): Unit = {
      service.getParameter.removeIf(new Predicate[TypeValueType] {
        def test(v: TypeValueType) = v.getType == PROTECTION_PARAMETER_TYPE
      })
      protection.foreach { protection =>
        service.getParameter.add(new TypeValueType().withType(PROTECTION_PARAMETER_TYPE).withValue(protection.toString))
      }
    }

    def protectionType: Option[ProtectionType] = {
      service.getParameter.asScala
        .find { _.getType == PROTECTION_PARAMETER_TYPE }
        .map(_.getValue)
        .flatMap(ProtectionType.fromString)
    }

    def sourceStp: Stp = Stp.fromString(service.getSourceSTP).getOrElse {
      throw new IllegalArgumentException(s"invalid source STP ${service.getSourceSTP}")
    }

    def destStp: Stp = Stp.fromString(service.getDestSTP).getOrElse {
      throw new IllegalArgumentException(s"invalid destination STP ${service.getDestSTP}")
    }
  }

  implicit class ReservationRequestCriteriaTypeOps(requestCriteria: ReservationRequestCriteriaType) {
    def toModifiedConfirmCriteria(previouslyCommittedCriteria: ReservationConfirmCriteriaType): Try[ReservationConfirmCriteriaType] = for {
      committedP2P <- previouslyCommittedCriteria.getPointToPointService().toTry(s"point2point service is missing from committed criteria")
      requestP2P <- requestCriteria.getPointToPointService().toTry(s"point2point service is missing from request criteria")
    } yield {
      val confirmP2P = committedP2P.shallowCopy.withCapacity(requestP2P.getCapacity)

      val schedule = Option(requestCriteria.getSchedule) getOrElse previouslyCommittedCriteria.getSchedule
      if (schedule.getStartTime eq null) schedule.setStartTime(previouslyCommittedCriteria.getSchedule.getStartTime)

      val confirmCriteria = new ReservationConfirmCriteriaType()
        .withAny(previouslyCommittedCriteria.getAny)
        .withServiceType(previouslyCommittedCriteria.getServiceType)
        .withSchedule(schedule)
        .withVersion(if (requestCriteria.getVersion eq null) previouslyCommittedCriteria.getVersion + 1 else requestCriteria.getVersion)
      confirmCriteria.withPointToPointService(confirmP2P)
      confirmCriteria.getOtherAttributes.putAll(previouslyCommittedCriteria.getOtherAttributes)
      confirmCriteria
    }

    def toInitialConfirmCriteria(fullySpecifiedSource: String, fullySpecifiedDest: String): Try[ReservationConfirmCriteriaType] =
      toModifiedConfirmCriteria(new ReservationConfirmCriteriaType()
        .withAny(requestCriteria.getAny)
        .withSchedule(new ScheduleType())
        .withServiceType(requestCriteria.getServiceType)
        .withPointToPointService(requestCriteria.getPointToPointService().get.shallowCopy.withSourceSTP(fullySpecifiedSource).withDestSTP(fullySpecifiedDest))
        .withVersion(0))

    /** Schedule is optional. */
    def schedule: Option[ScheduleType] = Option(requestCriteria.getSchedule)

    def version: Option[Int] = if (requestCriteria.getVersion eq null) None else Some(requestCriteria.getVersion.intValue)
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

  implicit val P2PServiceBaseTypeShallowCopyable = ShallowCopyable.build { a: P2PServiceBaseType =>
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

  implicit val ScheduleTypeShallowCopyable = ShallowCopyable.build { a: ScheduleType =>
    new ScheduleType().withStartTime(a.getStartTime).withEndTime(a.getEndTime)
  }

  private[messages] implicit class RichString(str: String) {
    def uncapitalize: String = str.take(1).toLowerCase + str.drop(1)
  }
}
