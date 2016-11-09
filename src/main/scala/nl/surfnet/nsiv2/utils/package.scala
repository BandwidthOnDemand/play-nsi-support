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
import java.time._
import java.util.GregorianCalendar
import javax.xml.datatype.{ DatatypeFactory, XMLGregorianCalendar }
import nl.surfnet.bod.nsi.Nillable
import nl.surfnet.nsiv2.messages._
import org.ogf.schemas.nsi._2013._12.connection.types.ScheduleType
import scala.util.{ Failure, Success, Try }


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

  implicit class NillableOps[T] (nillable: Nillable[T]) {
    private def asJavaFunction1[A, B](f: A => B) = new java.util.function.Function[A, B] { def apply(a: A) = f(a) }
    private def asJavaSupplier[A](f: => A) = new java.util.function.Supplier[A] { def get = f }

    def map2[A] (f: T => A) = nillable map asJavaFunction1(f)
    def fold2[A] (p: T => A, a: => A, n: => A) = nillable.fold(asJavaFunction1(p), asJavaSupplier(a), asJavaSupplier(n))
    def orElse2 (f: => Nillable[T]) = nillable orElse asJavaSupplier(f)
    def toOption (nil: => Option[T]): Option[T] = fold2(Some(_), None, nil)
  }

  implicit class ScheduleTypeOps(schedule: ScheduleType) {
    def startTime: Nillable[Instant] = Option(schedule).fold(Nillable.absent[Instant])(_.getStartTime.map2(_.toInstant))
    def endTime: Nillable[Instant] = Option(schedule).fold(Nillable.absent[Instant])(_.getEndTime.map2(_.toInstant))
  }
}
