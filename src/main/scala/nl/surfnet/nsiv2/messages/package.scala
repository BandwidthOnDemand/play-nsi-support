package nl.surfnet.nsiv2

import java.net.URI
import javax.xml.bind.JAXBElement
import javax.xml.datatype.{ DatatypeFactory, XMLGregorianCalendar }
import javax.xml.namespace.QName
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.joda.time.{ DateTime, DateTimeZone }
import play.api.data.validation.ValidationError
import play.api.libs.json._
import scala.util.Try
import org.ogf.schemas.nsi._2013._12.services.types.TypeValueType
import java.util.function.Predicate
import scala.collection.JavaConverters._
import play.api.libs.functional.syntax._

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
    def withPointToPointService(service: P2PServiceBaseType): A = {
      val element = PointToPointObjectFactory.createP2Ps(service)

      // FIXME replace if already exists?
      HasXmlAny[A].setAny(a, Seq(element))

      a
    }

    def getPointToPointService(): Option[P2PServiceBaseType] = HasXmlAny[A].getAny(a).collectFirst {
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
  }

  private[messages] implicit class RichString(str: String) {
    def uncapitalize: String = str.take(1).toLowerCase + str.drop(1)
  }
}
