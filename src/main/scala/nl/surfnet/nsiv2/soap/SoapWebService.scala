package nl.surfnet.nsiv2.soap

import java.net.URI
import play.api.http.ContentTypes
import play.api.mvc._
import play.api.mvc.Results._
import scala.util.Try
import scalax.io.Resource
import scala.concurrent.Future

trait SoapWebService {

  // FIXME resolve relative paths correctly. The current implementation works for NSI and MTOSI, but that's about it.

  val WsdlRoot: String
  val WsdlPath: String
  val WsdlBasename: String

  def serviceUrl: String

  def wsdl = Action.async { implicit request =>
    if (request.getQueryString("wsdl").isDefined || request.getQueryString("WSDL").isDefined)
      wsdlOrXsd(WsdlBasename)(request)
    else
      Future.successful(Results.NotFound)
  }

  def wsdlOrXsd(name: String) = serveWsdl(name) { _ => replaceSoapAddress }

  private def replaceSoapAddress(wsdl: String) = {
    wsdl.replaceAll(
      """(?i)<\s*soap:address\s+location\s*=\s*['"](.*)['"]\s*/>""",
      s"""<soap:address location="$serviceUrl" />""")
  }

  private def serveWsdl(path: String)(transform: RequestHeader => String => String) = Action { implicit request =>
    readClasspathWsdl(path).map(transform(request)).map { body =>
      Ok(body).as(ContentTypes.XML)
    }.getOrElse {
      NotFound(s"Resource '$path' not found")
    }
  }

  private def readClasspathResource(resource: String): Option[String] =
    Try(Resource.fromClasspath(resource).string(scalax.io.Codec.UTF8)).toOption

  private def readClasspathWsdl(name: String): Option[String] = {
    val resolved = Try(URI.create(WsdlPath).resolve(name).toString).toOption
    resolved.filterNot(_ contains "..").filterNot(_ startsWith "/").flatMap(name => readClasspathResource(s"$WsdlRoot/$name"))
  }
}
