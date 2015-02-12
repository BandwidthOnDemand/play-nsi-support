/*
 * Copyright (c) 2012, 2013, 2014, 2015 SURFnet BV
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
