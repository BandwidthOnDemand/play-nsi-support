package nl.surfnet.nsiv2

import java.net.URI
import javax.xml.datatype.XMLGregorianCalendar
import org.joda.time.DateTime
import scala.util.{ Failure, Success, Try }
import java.util.GregorianCalendar
import java.time.ZoneId
import java.time.ZonedDateTime
import javax.xml.datatype.DatatypeFactory

package object utils {
  def classpathResourceUri(name: String): URI = {
    val classLoader = Thread.currentThread().getContextClassLoader
    val resource = classLoader.getResource(name)
    if (resource != null) resource.toURI
    else throw new IllegalArgumentException(f"classpath resource '$name' not found")
  }

  implicit class AnyOps[A](a: A) {
    def tap[B](f: A => B): A = { f(a); a }
    def pp: A = { Console.err.println(a); a }
    def pp(prefix: String): A = { Console.err.println(s"$prefix: $a"); a }
  }

  implicit class OptionOps[A](value: Option[A]) {
    def toTry(ifNone: => Throwable): Try[A] = value.map(Success(_)).getOrElse(Failure(ifNone))
    def toTry(ifNone: String): Try[A] = value.map(Success(_)).getOrElse(Failure(ErrorMessage(ifNone)))
  }

  implicit class OptionTryOps[A](value: Option[Try[A]]) {
    def sequence: Try[Option[A]] = value match {
      case None    => Success(None)
      case Some(t) => t.map(Some(_))
    }
  }

  implicit class TryOps[A](a: Try[A]) {
    def toEither: Either[Throwable, A] = a match {
      case Failure(t) => Left(t)
      case Success(a) => Right(a)
    }
  }

  implicit object XmlGregorianCalendarOrdering extends Ordering[XMLGregorianCalendar] {
    def compare(x: XMLGregorianCalendar, y: XMLGregorianCalendar): Int = x compare y
  }
  implicit object DateTimeOrdering extends Ordering[DateTime] {
    def compare(x: DateTime, y: DateTime): Int = x compareTo y
  }

  implicit class ReadableInstantOps(instant: org.joda.time.ReadableInstant) {
    def toSqlTimestamp = new java.sql.Timestamp(instant.getMillis)
    def toJavaInstant = java.time.Instant.ofEpochMilli(instant.getMillis)
  }

  val utc = ZoneId.of("Z")

  implicit class InstantOps(instant: java.time.Instant) {
    def toSqlTimestamp = new java.sql.Timestamp(instant.toEpochMilli)

    def toXMLFormat(zoneId: ZoneId = utc) = toXMLGregorianCalendar(zoneId).toXMLFormat

    def toZonedDateTime(zoneId: ZoneId = utc) = ZonedDateTime.ofInstant(instant, zoneId)

    def toXMLGregorianCalendar(zoneId: ZoneId = utc) = {
      val cal = GregorianCalendar.from(instant.toZonedDateTime(zoneId))
      DatatypeFactory.newInstance.newXMLGregorianCalendar(cal)
    }
  }
}
