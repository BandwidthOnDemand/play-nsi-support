package nl.surfnet.nsiv2.soap

import org.specs2._

import scala.concurrent.Future
import play.api.mvc._
import play.api.http.HeaderNames.CONTENT_TYPE
import play.api.test._
import play.api.test.Helpers._
import javax.xml.soap._
import java.io.File
import nl.surfnet.nsiv2.messages._
import NsiSoapConversions.NsiXmlDocumentConversion
import akka.stream._, scaladsl._
import akka.util.ByteString

@org.junit.runner.RunWith(classOf[runner.JUnitRunner])
class ExtraBodyParsersSpec extends mutable.Specification
    with play.api.test.DefaultAwaitTimeout
    with play.api.test.FutureAwaits {

  class Fixture extends WithApplication() {
    implicit lazy val executionContext = app.actorSystem.dispatcher
    lazy val controllerComponents = stubControllerComponents()
    implicit lazy val actionBuilder = controllerComponents.actionBuilder
    implicit lazy val bodyParsers = stubPlayBodyParsers
    lazy val extraBodyParsers = new ExtraBodyParsers

    def run[A, B](future: Future[Either[A, B]]): Either[Future[A], B] =
      await(future).fold(a => Left(Future.successful(a)), b => Right(b))
  }

  "SoapBodyParser" should {

    "give UnsupportedMediaType when wrong content-type is set" in new Fixture {
      val request = FakeRequest().withHeaders(CONTENT_TYPE -> "text/html")
      val result = run(extraBodyParsers.soap(NsiXmlDocumentConversion).apply(request).run)

      result must beLeft.like { case result => status(result) must beEqualTo(415) }
    }

    "give InternalServerError with a SOAP fault when the soap message is not valid" in new Fixture {
      val result = run(extraBodyParsers.soap(NsiXmlDocumentConversion).apply(FakeSoapRequest()).run(Source(List(ByteString("<nonSoap></nonSoap>")))))

      result must beLeft.like { case result =>
        status(result) must beEqualTo(500)
        contentType(result) must beEqualTo(Some(SOAPConstants.SOAP_1_1_CONTENT_TYPE))
        contentAsString(result) must contain("<S:Fault")
      }
    }

    "give EntityTooLarge when the input is to large" in new Fixture {
      val result = run(extraBodyParsers.soap(NsiXmlDocumentConversion, 10).apply(FakeSoapRequest()).run(Source(List(ByteString("<test></test>")))))

      result must beLeft.like { case result => status(result) must beEqualTo(413) }
    }

    "give a SOAP message for a valid request" in new Fixture {
      val result = run(extraBodyParsers.soap(NsiXmlDocumentConversion).apply(FakeSoapRequest()).run(FileIO.fromPath(new File("src/test/resources/reserve.xml").toPath)))

      result must beRight
    }
  }

  "NsiProviderParser" should {

    "give NSI Initial Reserve for a valid reserve request" in new Fixture {
      val result = run(extraBodyParsers.nsiProviderOperation.apply(FakeSoapRequest()).run(FileIO.fromPath(new File("src/test/resources/reserve.xml").toPath)))

      result must beRight.like { case NsiProviderMessage(_, _: InitialReserve) => ok }
    }

    "give NSI Modify Reserve for a valid reserve request" in new Fixture {
      val result = run(extraBodyParsers.nsiProviderOperation.apply(FakeSoapRequest()).run(FileIO.fromPath(new File("src/test/resources/reserve_modify.xml").toPath)))

      result must beRight.like { case NsiProviderMessage(_, _: ModifyReserve) => ok }
    }

    "give InternalServerError when NSI Reserve contains extra xml" in new Fixture {
      val result = run(extraBodyParsers.nsiProviderOperation.apply(FakeSoapRequest()).run(FileIO.fromPath(new File("src/test/resources/reserve_additional_xml.xml").toPath)))

      result must beLeft.like {
        case result =>
          status(result) must beEqualTo(500)
          contentType(result) must beEqualTo(Some(SOAPConstants.SOAP_1_1_CONTENT_TYPE))
          contentAsString(result) must contain("<faultstring>Error parsing SOAP request: org.xml.sax.SAXParseException; lineNumber: 13; columnNumber: 19; cvc-complex-type.2.4.a: Invalid content was found starting with element 'test'. One of '{connectionId, globalReservationId, description, criteria}' is expected.</faultstring>")
      }
    }

    "give InternalServerError when the NSI headers are missing" in new Fixture {
      val result = run(extraBodyParsers.nsiProviderOperation.apply(FakeSoapRequest()).run(FileIO.fromPath(new File("src/test/resources/reserve_without_headers.xml").toPath)))

      result must beLeft.like {
        case result =>
          status(result) must beEqualTo(500)
          contentType(result) must beEqualTo(Some(SOAPConstants.SOAP_1_1_CONTENT_TYPE))
          contentAsString(result) must contain("<faultstring>Error parsing NSI message in SOAP request: missing NSI headers</faultstring>")
      }
    }

    "give InternalServerError when there are multiple NSI headers" in new Fixture {
      val result = run(extraBodyParsers.nsiProviderOperation.apply(FakeSoapRequest()).run(FileIO.fromPath(new File("src/test/resources/reserve_with_duplicate_headers.xml").toPath)))

      result must beLeft.like {
        case result =>
          status(result) must beEqualTo(500)
          contentType(result) must beEqualTo(Some(SOAPConstants.SOAP_1_1_CONTENT_TYPE))
          contentAsString(result) must contain("<faultstring>Error parsing NSI message in SOAP request: multiple elements 'http://schemas.ogf.org/nsi/2013/12/framework/headers:nsiHeader' in 'Header', expected exactly one</faultstring>")
      }
    }
  }

  "NsiRequesterParser" should {
    "give NSI Reserve Commit for a valid reserve confirmed request" in new Fixture {
      val result = run(extraBodyParsers.nsiRequesterOperation.apply(FakeSoapRequest()).run(FileIO.fromPath(new File("src/test/resources/reserveconfirmed.xml").toPath)))

      result must beRight.like { case NsiRequesterMessage(_, _: ReserveConfirmed) => ok }
    }
  }

  object FakeSoapRequest {
    def apply(): FakeRequest[AnyContentAsEmpty.type] = {
      FakeRequest("POST", "/", FakeHeaders(), AnyContentAsEmpty).withHeaders(CONTENT_TYPE -> SOAPConstants.SOAP_1_1_CONTENT_TYPE)
    }
  }
}
