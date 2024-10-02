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

import java.net.URI
import play.api.http.ContentTypes
import play.api.mvc.*
import play.api.mvc.Results.*
import scala.util.Try
import scala.concurrent.Future

trait SoapWebService {

  // FIXME resolve relative paths correctly. The current implementation works for NSI and MTOSI, but that's about it.

  val WsdlRoot: String
  val WsdlPath: String
  val WsdlBasename: String

  def actionBuilder: DefaultActionBuilder

  def serviceUrl: String

  def wsdl: Action[AnyContent] = actionBuilder.async { implicit request =>
    if request.getQueryString("wsdl").isDefined || request.getQueryString("WSDL").isDefined then
      wsdlOrXsd(WsdlBasename)(request)
    else Future.successful(Results.NotFound)
  }

  def wsdlOrXsd(name: String): Action[AnyContent] = serveWsdl(name) { _ => replaceSoapAddress }

  private def replaceSoapAddress(wsdl: String) = {
    wsdl.replaceAll(
      """(?i)<\s*soap:address\s+location\s*=\s*['"](.*)['"]\s*/>""",
      s"""<soap:address location="$serviceUrl" />"""
    )
  }

  private def serveWsdl(
      path: String
  )(transform: RequestHeader => String => String): Action[AnyContent] =
    actionBuilder { implicit request =>
      readClasspathWsdl(path)
        .map(transform(request))
        .map { body =>
          Ok(body).as(ContentTypes.XML)
        }
        .getOrElse {
          NotFound(s"Resource '$path' not found")
        }
    }

  private def readClasspathResource(resource: String): Option[String] = Try {
    scala.io.Source.fromResource(resource).mkString
  }.toOption

  private def readClasspathWsdl(name: String): Option[String] = {
    val resolved = Try(URI.create(WsdlPath).resolve(name).toString).toOption
    resolved
      .filterNot(_ contains "..")
      .filterNot(_ startsWith "/")
      .flatMap(name => readClasspathResource(s"$WsdlRoot/$name"))
  }
}
