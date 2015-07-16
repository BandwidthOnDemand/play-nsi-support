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

import com.google.common.collect.ImmutableRangeSet
import com.google.common.collect.Range
import com.google.common.collect.TreeRangeSet
import java.net.URLDecoder
import java.net.URLEncoder
import java.util.regex.Pattern
import scala.collection.JavaConverters._
import scala.collection.immutable.SortedMap
import scala.util.Try
import play.utils.UriEncoding

final class VlanRange(private val range: ImmutableRangeSet[Integer]) {
  require(!range.isEmpty, "VLAN range cannot be empty")

  def isSingleton: Boolean = {
    val span = range.span
    span.upperEndpoint() == span.lowerEndpoint()
  }
  def isSubsetOf(that: VlanRange): Boolean = that.range enclosesAll this.range

  def lowerBound: Int = range.span().lowerEndpoint()
  def upperBound: Int = range.span().upperEndpoint()

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

case class Stp(identifier: String, labels: SortedMap[String, Option[String]] = SortedMap.empty) {
  require(identifier.nonEmpty, "identifier must be non-empty")
  require(labels.forall(_._1.nonEmpty), "label types must be non-empty")

  def withoutLabels = copy(labels = SortedMap.empty)
  def withLabel(labelType: String, labelValue: String) = copy(labels = labels + (labelType -> Some(labelValue)))

  def vlan: Option[VlanRange] = labels.getOrElse("vlan", None).flatMap(VlanRange.fromString)

  def serverVlan: Option[VlanRange] = labels.getOrElse("s-vlan", None).flatMap(VlanRange.fromString)

  def isClientVlanCompatibleWith(target: Stp): Boolean = (this.vlan, target.vlan) match {
    case (None, _)                        => true
    case (Some(specified), Some(allowed)) => specified isSubsetOf allowed
    case _                                => false
  }

  def isServerVlanCompatibleWith(target: Stp): Boolean = (this.serverVlan, target.serverVlan) match {
    case (None, None)                     => true
    case (Some(specified), Some(allowed)) => specified isSubsetOf allowed
    case _                                => false
  }

  def isCompatibleWith(that: Stp) = this.identifier == that.identifier && this.isClientVlanCompatibleWith(that) && this.isServerVlanCompatibleWith(that)

  override def toString = UriEncoding.encodePathSegment(identifier, "UTF-8") ++ queryString

  private def queryString = if (labels.isEmpty) "" else labels.iterator.map {
    case (label, None)        => URLEncoder.encode(label, "UTF-8")
    case (label, Some(value)) => s"${URLEncoder.encode(label, "UTF-8")}=${URLEncoder.encode(value, "UTF-8")}"
  }.mkString("?", "&", "")
}

object Stp {
  type Label = (String, Option[String])

  import scala.math.Ordering.Implicits._
  implicit val StpOrdering: Ordering[Stp] = Ordering.by(stp => (stp.identifier, stp.labels.toStream))

  private val LabelPattern = "([^=]*)(?:=([^=]*))?".r

  def fromString(s: String): Option[Stp] = {
    def parseLabel(label: String): Option[Label] = label match {
      case LabelPattern(labelType, labelValue) if labelType.nonEmpty =>
        Some((URLDecoder.decode(labelType, "UTF-8"), Option(labelValue).map(URLDecoder.decode(_, "UTF-8"))))
      case _ =>
        None
    }

    s.split(Pattern.quote("?")) match {
      case Array(identifier) if identifier.nonEmpty =>
        Some(Stp(UriEncoding.decodePath(identifier, "UTF-8")))
      case Array(identifier, queryString) if identifier.nonEmpty =>
        val parsedLabels = queryString.split(Pattern.quote("&")).map(parseLabel)
        val labels = if (parsedLabels contains None) None else Some(parsedLabels.map(_.get))
        labels.map { l => 
          Stp(UriEncoding.decodePath(identifier, "UTF-8"), SortedMap(l: _*))
        }
      case _ =>
        None
    }
  }
}
