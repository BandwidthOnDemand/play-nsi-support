package nl.surfnet.nsiv2
package messages

import java.util.regex.Pattern
import com.google.common.collect.ImmutableRangeSet
import com.google.common.collect.Range
import com.google.common.collect.TreeRangeSet
import scala.collection.JavaConverters._
import scala.util.Try

final class VlanRange(private val range: ImmutableRangeSet[Integer]) {
  def subsetOf(that: VlanRange): Boolean = that.range enclosesAll this.range

  override def equals(o: Any) = o match {
    case that: VlanRange => this.range == that.range
    case _ => false
  }

  override def hashCode = range.hashCode

  override def toString = range.asRanges().asScala.map { range =>
    (range.lowerEndpoint(), range.upperEndpoint()) match {
      case (lower, upper) if lower == upper => f"$lower%d"
      case (lower, upper) => f"$lower%d-$upper%d"
    }
  }.mkString(",")
}
object VlanRange {
  private final val ALLOWED_SYNTAX = Pattern.compile("\\s*\\d+(\\s*-\\s*\\d+)?(\\s*,\\s*\\d+(\\s*-\\s*\\d+)?)*\\s*")
  private final val RANGE_PATTERN = "(\\d+)-(\\d+)".r
  private final val SINGLETON_PATTERN = "(\\d+)".r

  def apply(ranges: Seq[Range[Integer]]): VlanRange = {
    val set = TreeRangeSet.create[Integer]
    ranges.foreach(set.add)
    new VlanRange(ImmutableRangeSet.copyOf(set))
  }

  def fromString(s: String): Option[VlanRange] = if (!ALLOWED_SYNTAX.matcher(s).matches()) None else Try {
    val ranges = s.replaceAll("\\s+", "").split(",").map {
      case RANGE_PATTERN(lower, upper) => Range.closed(Integer.valueOf(lower), Integer.valueOf(upper))
      case SINGLETON_PATTERN(value) => Range.singleton(Integer.valueOf(value))
    }
    VlanRange(ranges)
  }.toOption
}

case class Stp(identifier: String, label: Option[Stp.Label] = None) {
  def withoutLabel = copy(label = None)
  def withLabel(labelType: String, labelValue: String) = copy(label = Some(Stp.Label(labelType, Some(labelValue))))

  def vlan: Option[VlanRange] = label.filter(_.labelType == "vlan").flatMap(_.labelValue).flatMap(VlanRange.fromString)

  def isCompatibleWith(that: Stp) = (this.identifier == that.identifier) && ((this.vlan, that.vlan) match {
    case (None, _) => true
    case (Some(vlan), None) => false
    case (Some(thisVlan), Some(thatVlan)) => thisVlan subsetOf thatVlan
  })

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
