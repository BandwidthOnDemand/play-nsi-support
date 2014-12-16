package nl.surfnet.nsiv2.messages

import java.util.regex.Pattern

case class Stp(identifier: String, label: Option[Stp.Label] = None) {
  def withoutLabel = copy(label = None)
  def withLabel(labelType: String, labelValue: String) = copy(label = Some(Stp.Label(labelType, Some(labelValue))))

  def vlan: Option[String] = label.filter(_.labelType == "vlan").flatMap(_.labelValue)

  override def toString = identifier ++ label.map(label => "?" ++ label.toString).getOrElse("")
}
object Stp {

  case class Label(labelType: String, labelValue: Option[String]) {
    override def toString = labelType ++ labelValue.map("=" ++ _).getOrElse("")
  }
  object Label {
    implicit val LabelOrdering: Ordering[Label] = Ordering.by(label => (label.labelType, label.labelValue))
  }

  implicit val StpOrdering: Ordering[Stp] = Ordering.by(stp => (stp.identifier, stp.label))

  private val LabelPattern = "([^=]*)(?:=([^=]*))?".r

  def fromString(s: String): Option[Stp] = {
    def parseLabel(label: String): Option[Label] = label match {
      case LabelPattern(labelType, labelValue) =>
        Some(Label(labelType, Option(labelValue)))
      case _ =>
        None
    }

    s.split(Pattern.quote("?")) match {
      case Array(identifier) =>
        Some(new Stp(identifier))
      case Array(identifier, label) =>
        parseLabel(label).map(label => new Stp(identifier, Some(label)))
      case _ =>
        None
    }
  }
}
