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

import org.ogf.schemas.nsi._2013._12.connection.types.ScheduleType
import org.ogf.schemas.nsi._2013._12.services.point2point.P2PServiceBaseType

trait ShallowCopyable[A]:
  extension (a: A) def shallowCopy: A

given ShallowCopyable[P2PServiceBaseType] with
  extension (a: P2PServiceBaseType)
    def shallowCopy =
      new P2PServiceBaseType()
        .withAny(a.getAny)
        .withCapacity(a.getCapacity)
        .withDestSTP(a.getDestSTP)
        .withDirectionality(a.getDirectionality)
        .withEro(a.getEro)
        .withParameter(a.getParameter)
        .withSourceSTP(a.getSourceSTP)
        .withSymmetricPath(a.isSymmetricPath)

given ShallowCopyable[ScheduleType] with
  extension (a: ScheduleType)
    def shallowCopy = new ScheduleType().withStartTime(a.getStartTime).withEndTime(a.getEndTime)
