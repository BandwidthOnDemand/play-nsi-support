package nl.surfnet.nsiv2.messages

import org.ogf.schemas.nsi._2013._12.connection.types.{ChildSummaryType, QuerySummaryResultCriteriaType, ReservationConfirmCriteriaType, ReservationRequestCriteriaType}

/**
 * Type class for JAXB generated types that have an XML any element.
 */
trait HasXmlAny[A] {
  def getAny(a: A): java.util.List[AnyRef]
}
object HasXmlAny {
  def apply[A](implicit hasXmlAny: HasXmlAny[A]) = hasXmlAny

  private def build[A](get: A => java.util.List[AnyRef]): HasXmlAny[A] = new HasXmlAny[A] {
    def getAny(a: A) = get(a)
  }

  implicit val ChildSummaryType: HasXmlAny[ChildSummaryType] = build(_.getAny)
  implicit val QuerySummaryResultCriteriaType: HasXmlAny[QuerySummaryResultCriteriaType] = build(_.getAny)
  implicit val ReservationConfirmCriteriaType: HasXmlAny[ReservationConfirmCriteriaType] = build(_.getAny)
  implicit val ReservationRequestCriteriaType: HasXmlAny[ReservationRequestCriteriaType] = build(_.getAny)
}
