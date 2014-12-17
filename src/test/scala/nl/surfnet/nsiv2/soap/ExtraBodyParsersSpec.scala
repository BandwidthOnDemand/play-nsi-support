package nl.surfnet.nsiv2.soap

import org.specs2._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.api.mvc._
import play.api.http.HeaderNames.CONTENT_TYPE
import play.api.test._
import play.api.test.Helpers._
import play.api.libs.iteratee._
import javax.xml.soap._
import java.io.File
import nl.surfnet.nsiv2.messages._
import java.util.concurrent.TimeUnit
import NsiSoapConversions.NsiXmlDocumentConversion

@org.junit.runner.RunWith(classOf[runner.JUnitRunner])
class ExtraBodyParsersSpec extends mutable.Specification
    with play.api.test.DefaultAwaitTimeout
    with play.api.test.FutureAwaits {

  import ExtraBodyParsers._

  private def run[A, B](future: Future[Either[A, B]]): Either[Future[A], B] =
    await(future).fold(a => Left(Future.successful(a)), b => Right(b))

  "SoapBodyParser" should {

    "give UnsupportedMediaType when wrong content-type is set" in {
      val request = FakeRequest().withHeaders(CONTENT_TYPE -> "text/html")
      val result = run(soap(NsiXmlDocumentConversion).apply(request).run)

      result must beLeft.like { case result => status(result) must beEqualTo(415) }
    }

    "give InternalServerError with a SOAP fault when the soap message is not valid" in {
      val result = run(Enumerator("<nonSoap></nonSoap>".getBytes) |>>> soap(NsiXmlDocumentConversion).apply(FakeSoapRequest()))

      result must beLeft.like { case result =>
        status(result) must beEqualTo(500)
        contentType(result) must beEqualTo(Some(SOAPConstants.SOAP_1_1_CONTENT_TYPE))
        contentAsString(result) must contain("<S:Fault")
      }
    }

    "give EntityTooLarge when the input is to large" in {
      val result = run(Enumerator("<test></test>".getBytes) |>>> soap(NsiXmlDocumentConversion, 10).apply(FakeSoapRequest()))

      result must beLeft.like { case result => status(result) must beEqualTo(413) }
    }

    "give a SOAP message for a valid request" in {
      val result = run(Enumerator.fromFile(new File("src/test/resources/reserve.xml")) |>>> soap(NsiXmlDocumentConversion).apply(FakeSoapRequest()))

      result must beRight
    }
  }

  "NsiProviderParser" should {

    "give NSI Initial Reserve for a valid reserve request" in {
      val result = run(Enumerator.fromFile(new File("src/test/resources/reserve.xml")) |>>> nsiProviderOperation.apply(FakeSoapRequest()))

      result must beRight.like { case NsiProviderMessage(_, _: InitialReserve) => ok }
    }

    "give NSI Modify Reserve for a valid reserve request" in {
      val result = run(Enumerator.fromFile(new File("src/test/resources/reserve_modify.xml")) |>>> nsiProviderOperation.apply(FakeSoapRequest()))

      result must beRight.like { case NsiProviderMessage(_, _: ModifyReserve) => ok }
    }

    "give InternalServerError when NSI Reserve contains extra xml" in {
      val result = run(Enumerator.fromFile(new File("src/test/resources/reserve_additional_xml.xml")) |>>> nsiProviderOperation.apply(FakeSoapRequest()))

      result must beLeft.like {
        case result =>
          status(result) must beEqualTo(500)
          contentType(result) must beEqualTo(Some(SOAPConstants.SOAP_1_1_CONTENT_TYPE))
          contentAsString(result) must contain("<faultstring>Error parsing SOAP request: org.xml.sax.SAXParseException; lineNumber: 13; columnNumber: 19; cvc-complex-type.2.4.a: Invalid content was found starting with element 'test'. One of '{connectionId, globalReservationId, description, criteria}' is expected.</faultstring>")
      }
    }

    "give InternalServerError when the NSI headers are missing" in {
      val result = run(Enumerator.fromFile(new File("src/test/resources/reserve_without_headers.xml")) |>>> nsiProviderOperation.apply(FakeSoapRequest()))

      result must beLeft.like {
        case result =>
          status(result) must beEqualTo(500)
          contentType(result) must beEqualTo(Some(SOAPConstants.SOAP_1_1_CONTENT_TYPE))
          contentAsString(result) must contain("<faultstring>Error parsing NSI message in SOAP request: missing NSI headers</faultstring>")
      }
    }

    "give InternalServerError when there are multiple NSI headers" in {
      val result = run(Enumerator.fromFile(new File("src/test/resources/reserve_with_duplicate_headers.xml")) |>>> nsiProviderOperation.apply(FakeSoapRequest()))

      result must beLeft.like {
        case result =>
          status(result) must beEqualTo(500)
          contentType(result) must beEqualTo(Some(SOAPConstants.SOAP_1_1_CONTENT_TYPE))
          contentAsString(result) must contain("<faultstring>Error parsing NSI message in SOAP request: multiple elements 'http://schemas.ogf.org/nsi/2013/12/framework/headers:nsiHeader' in 'Header', expected exactly one</faultstring>")
      }
    }
  }

  "NsiRequesterParser" should {
    "give NSI Reserve Commit for a valid reserve confirmed request" in {
      val result = run(Enumerator.fromFile(new File("src/test/resources/reserveconfirmed.xml")) |>>> nsiRequesterOperation.apply(FakeSoapRequest()))

      result must beRight.like { case NsiRequesterMessage(_, _: ReserveConfirmed) => ok }
    }
  }

  object FakeSoapRequest {
    def apply(): FakeRequest[AnyContentAsEmpty.type] = {
      FakeRequest("POST", "/", FakeHeaders(), AnyContentAsEmpty).withHeaders(CONTENT_TYPE -> SOAPConstants.SOAP_1_1_CONTENT_TYPE)
    }
  }
}
