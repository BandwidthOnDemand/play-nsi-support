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

import javax.xml.datatype.XMLGregorianCalendar
import org.ogf.schemas.nsi._2013._12.connection.types.*
import org.ogf.schemas.nsi._2013._12.framework.types.ServiceExceptionType

sealed trait NsiAcknowledgement extends NsiOperation {
  final def action: String = this.getClass.getSimpleName
}
case class GenericAck() extends NsiAcknowledgement
case class ReserveResponse(connectionId: String) extends NsiAcknowledgement
case class ServiceException(exception: ServiceExceptionType) extends NsiAcknowledgement
case class QuerySummarySyncConfirmed(
    results: Seq[QuerySummaryResultType],
    lastModified: Option[XMLGregorianCalendar]
) extends NsiAcknowledgement
case class QueryNotificationSyncConfirmed(results: Seq[NotificationBaseType])
    extends NsiAcknowledgement
case class QueryResultSyncConfirmed(results: Seq[QueryResultResponseType])
    extends NsiAcknowledgement
case class ErrorAck(error: GenericErrorType) extends NsiAcknowledgement
