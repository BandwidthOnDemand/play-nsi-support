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

import jakarta.xml.bind.JAXBElement
import javax.xml.namespace.QName
import org.ogf.schemas.nsi._2013._12.connection.types.{
  ChildSummaryType,
  QuerySummaryResultCriteriaType,
  ReservationConfirmCriteriaType,
  ReservationRequestCriteriaType
}
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

/** Wraps an JAXB XML any list and tries to correctly implement equals and hashCode for data wrapped
  * in JAXBElement[_].
  *
  * Also provides utility methods for manipulating the XML elements.
  */
class XmlAny private (val elements: List[AnyRef]):

  override def equals(obj: Any): Boolean = obj match
    case that: XmlAny => unwrapJaxbElements(this.elements) == unwrapJaxbElements(that.elements)
    case _            => false

  override def hashCode(): Int = unwrapJaxbElements(elements).##

  override def toString(): String = unwrapJaxbElements(elements).mkString("XmlAny(", ",", ")")

  def find[T: ClassTag](nullElement: JAXBElement[T]): Seq[T] = elements collect {
    case XmlAny.Element(name, Some(value: T)) if name == nullElement.getName() => value
  }

  def findFirst[T: ClassTag](nullElement: JAXBElement[T]): Option[T] = elements collectFirst {
    case XmlAny.Element(name, Some(value: T)) if name == nullElement.getName() => value
  }

  def remove(nullElement: JAXBElement[?]): XmlAny = XmlAny(elements filterNot {
    case element: JAXBElement[?] => element.getName() == nullElement.getName
    case _                       => false
  })

  def update(element: JAXBElement[?]): XmlAny = XmlAny(element :: remove(element).elements)

  private def unwrapJaxbElements(any: Seq[AnyRef]) = any.map {
    case jaxb: JAXBElement[?] => XmlAny.Element(jaxb.getName(), jaxb.isNil(), jaxb.getValue())
    case other                => other
  }
end XmlAny
object XmlAny:
  def empty: XmlAny = apply(Nil)
  def apply(elements: Seq[AnyRef]): XmlAny = new XmlAny(elements.toList)
  def unapply(any: XmlAny): Option[List[AnyRef]] = Some(any.elements)

  case class Element[T] private[XmlAny] (name: QName, nil: Boolean, value: T)
  object Element:
    def unapply(any: Any): Option[(QName, Option[Any])] = any match
      case element: JAXBElement[?] =>
        Some((element.getName(), if element.isNil() then None else Option(element.getValue())))
      case _ =>
        None

/** Type class for JAXB generated types that have an XML any element.
  */
trait HasXmlAny[A]:
  extension (a: A)
    def getAny: java.util.List[AnyRef]

    def any: XmlAny = XmlAny(getAny.asScala.toList)

    def findAny[T: ClassTag](nullElement: JAXBElement[T]): List[T] = getAny.asScala.collect {
      case XmlAny.Element(name, Some(value: T)) if name == nullElement.getName() => value
    }.toList

    def findFirstAny[T: ClassTag](nullElement: JAXBElement[T]): Option[T] =
      getAny.asScala collectFirst {
        case XmlAny.Element(name, Some(value: T)) if name == nullElement.getName() => value
      }

    def removeAny(nullElement: JAXBElement[?]): Boolean =
      getAny.removeIf({
        case element: JAXBElement[?] => element.getName() == nullElement.getName
        case _                       => false
      })

    def updateAny(element: JAXBElement[?]): Unit =
      removeAny(element)
      getAny.add(element)
  end extension
end HasXmlAny
given HasXmlAny[ChildSummaryType] with
  extension (a: ChildSummaryType) def getAny = a.getAny
given HasXmlAny[QuerySummaryResultCriteriaType] with
  extension (a: QuerySummaryResultCriteriaType) def getAny = a.getAny
given HasXmlAny[ReservationConfirmCriteriaType] with
  extension (a: ReservationConfirmCriteriaType) def getAny = a.getAny
given HasXmlAny[ReservationRequestCriteriaType] with
  extension (a: ReservationRequestCriteriaType) def getAny = a.getAny
