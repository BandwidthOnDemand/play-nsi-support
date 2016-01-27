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
package nl.surfnet.nsiv2.soap

import scala.util.Try

/**
 * Defines an invertable conversion that can potentially fail.
 */
trait Conversion[A, B] { outer =>
  def apply(a: A): Try[B]
  def invert: Conversion[B, A]

  def andThen[C](that: Conversion[B, C]) = new Conversion[A, C] { inner =>
    override def apply(a: A): Try[C] = outer.apply(a).flatMap(that.apply)
    override val invert: Conversion[C, A] = new Conversion[C, A] {
      override def apply(c: C): Try[A] = that.invert.apply(c).flatMap(outer.invert.apply)
      override val invert = inner
    }
  }
}

object Conversion {
  def apply[A, B](implicit conversion: Conversion[A, B]) = conversion
  def build[A, B](to: A => Try[B])(from: B => Try[A]): Conversion[A, B] = new Conversion[A, B] { outer =>
    override def apply(a: A) = Try(to(a)).flatten
    override val invert = new Conversion[B, A] {
      override def apply(b: B) = Try(from(b)).flatten
      override val invert = outer
    }
  }

  def convert[A, B](a: A)(implicit conversion: Conversion[A, B]) = conversion(a)
  def invert[A, B](b: B)(implicit conversion: Conversion[A, B]) = conversion.invert(b)
}
