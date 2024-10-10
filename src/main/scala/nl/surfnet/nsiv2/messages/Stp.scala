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
package messages

import com.google.common.collect.ImmutableRangeSet
import com.google.common.collect.{Range as GRange}
import com.google.common.collect.TreeRangeSet
import java.net.URLDecoder
import java.net.URLEncoder
import java.util.regex.Pattern
import scala.jdk.CollectionConverters.*
import scala.collection.immutable.SortedMap
import scala.util.Try
import play.utils.UriEncoding
import com.google.common.collect.DiscreteDomain
import com.google.common.collect.BoundType

final class VlanRange(private val ranges: ImmutableRangeSet[Integer]):
  require(!ranges.isEmpty, "VLAN ranges cannot be empty")
  require(
    ranges.asRanges().asScala.forall { r =>
      (r.hasLowerBound() && r.lowerBoundType() == BoundType.CLOSED
      && r.hasUpperBound() && r.upperBoundType() == BoundType.CLOSED)
    },
    "all ranges must be closed"
  )

  private def span = ranges.span

  def isSingleton: Boolean = lowerBound == upperBound

  def isSubsetOf(that: VlanRange): Boolean = that.ranges enclosesAll this.ranges

  def lowerBound: Int = span.lowerEndpoint()
  def upperBound: Int = span.upperEndpoint()

  def intersect(that: VlanRange): Option[VlanRange] =
    val intersection = TreeRangeSet.create[Integer]
    this.ranges.asSet(DiscreteDomain.integers()).asScala.foreach { (vlan: Integer) =>
      if that.ranges.contains(vlan) then intersection.add(GRange.closedOpen(vlan, vlan + 1))
    }
    val closedRanges = TreeRangeSet.create[Integer]
    intersection.asRanges().asScala.foreach { range =>
      closedRanges.add(GRange.closed(range.lowerEndpoint, range.upperEndpoint - 1))
    }
    if closedRanges.isEmpty then None
    else Some(new VlanRange(ImmutableRangeSet.copyOf(closedRanges)))

  override def equals(o: Any): Boolean = o match
    case that: VlanRange => this.ranges == that.ranges
    case _               => false

  override def hashCode = ranges.hashCode

  override def toString: String = ranges
    .asRanges()
    .asScala
    .map { range =>
      (range.lowerEndpoint(), range.upperEndpoint()) match
        case (lower, upper) if lower == upper => f"$lower%d"
        case (lower, upper)                   => f"$lower%d-$upper%d"
    }
    .mkString(",")
end VlanRange
object VlanRange:
  private final val ALLOWED_SYNTAX =
    Pattern.compile("\\s*\\d+(\\s*-\\s*\\d+)?(\\s*,\\s*\\d+(\\s*-\\s*\\d+)?)*\\s*")
  private final val RANGE_PATTERN = "(\\d+)-(\\d+)".r
  private final val SINGLETON_PATTERN = "(\\d+)".r

  private[messages] def apply(ranges: Seq[GRange[Integer]]): VlanRange =
    val set = TreeRangeSet.create[Integer]
    ranges.foreach(set.add)
    new VlanRange(ImmutableRangeSet.copyOf(set))

  val all: VlanRange = apply(Seq(GRange.closed(1, 4094)))

  def singleton(v: Int): VlanRange = apply(Seq(GRange.singleton(v)))

  def range(range: Range): Option[VlanRange] =
    val end = if range.isInclusive then range.end else range.end - 1
    if range.start <= end && range.step == 1 then
      Some(VlanRange(Seq(GRange.closed(range.start, end))))
    else None

  def fromString(s: String): Option[VlanRange] =
    if !ALLOWED_SYNTAX.matcher(s).matches() then None
    else
      Try {
        val ranges = s.replaceAll("\\s+", "").split(",").map {
          case RANGE_PATTERN(lower, upper) =>
            GRange.closed(Integer.valueOf(lower), Integer.valueOf(upper))
          case SINGLETON_PATTERN(value) => GRange.singleton(Integer.valueOf(value))
        }
        VlanRange(ranges.toIndexedSeq)
      }.toOption
end VlanRange

case class Stp(identifier: String, labels: SortedMap[String, Option[String]] = SortedMap.empty):
  require(identifier.nonEmpty, "identifier must be non-empty")
  require(labels.forall(_._1.nonEmpty), "label types must be non-empty")

  def withoutLabels: Stp = copy(labels = SortedMap.empty)

  def withoutLabel(labelType: String): Stp = copy(labels = labels - labelType)

  def withLabel(labelType: String, labelValue: String): Stp =
    copy(labels = labels + (labelType -> Some(labelValue)))

  def vlan: Option[VlanRange] = labels.getOrElse("vlan", None).flatMap(VlanRange.fromString)

  def serverVlan: Option[VlanRange] = labels.getOrElse("s-vlan", None).flatMap(VlanRange.fromString)

  def isClientVlanCompatibleWith(target: Stp): Boolean = (this.vlan, target.vlan) match
    case (None, _)                        => true
    case (Some(specified), Some(allowed)) => specified isSubsetOf allowed
    case _                                => false

  def isServerVlanCompatibleWith(target: Stp): Boolean =
    (this.serverVlan, target.serverVlan) match
      case (None, None)                     => true
      case (Some(specified), Some(allowed)) => specified isSubsetOf allowed
      case _                                => false

  def isCompatibleWith(that: Stp): Boolean =
    this.identifier == that.identifier && this.isClientVlanCompatibleWith(that) && this
      .isServerVlanCompatibleWith(that)

  override def toString: String = UriEncoding.encodePathSegment(identifier, "UTF-8") ++ queryString

  private def queryString = if labels.isEmpty then ""
  else
    labels.iterator
      .map {
        case (label, None) => URLEncoder.encode(label, "UTF-8")
        case (label, Some(value)) =>
          s"${URLEncoder.encode(label, "UTF-8")}=${URLEncoder.encode(value, "UTF-8")}"
      }
      .mkString("?", "&", "")
end Stp

object Stp:
  type Label = (String, Option[String])

  import scala.math.Ordering.Implicits.*
  implicit val StpOrdering: Ordering[Stp] =
    Ordering.by(stp => (stp.identifier, stp.labels.toIndexedSeq))

  private val LabelPattern = "([^=]*)(?:=([^=]*))?".r

  def fromString(s: String): Option[Stp] =
    def parseLabel(label: String): Option[Label] = label match
      case LabelPattern(labelType, labelValue) if labelType.nonEmpty =>
        Some(
          (
            URLDecoder.decode(labelType, "UTF-8"),
            Option(labelValue).map(URLDecoder.decode(_, "UTF-8"))
          )
        )
      case _ =>
        None

    s.split(Pattern.quote("?")) match
      case Array(identifier) if identifier.nonEmpty =>
        Some(Stp(UriEncoding.decodePath(identifier, "UTF-8")))
      case Array(identifier, queryString) if identifier.nonEmpty =>
        val parsedLabels = queryString.split(Pattern.quote("&")).map(parseLabel)
        val labels = if parsedLabels contains None then None else Some(parsedLabels.map(_.get))
        labels.map { l =>
          Stp(UriEncoding.decodePath(identifier, "UTF-8"), SortedMap(l.toIndexedSeq*))
        }
      case _ =>
        None
  end fromString
end Stp
