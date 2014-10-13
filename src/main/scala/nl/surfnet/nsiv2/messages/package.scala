package nl.surfnet.nsiv2

import java.net.URI
import javax.xml.bind.JAXBElement
import javax.xml.datatype.{DatatypeFactory,XMLGregorianCalendar}
import javax.xml.namespace.QName
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType
import org.joda.time.{DateTime, DateTimeZone}

package object messages {
  type RequesterNsa = String
  type ConnectionId = String
  type GlobalReservationId = URI

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

  private [messages] implicit class RichString(str: String) {
    def uncapitalize: String = str.take(1).toLowerCase + str.drop(1)
  }
}
