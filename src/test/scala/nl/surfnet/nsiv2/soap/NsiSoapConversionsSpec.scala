package nl.surfnet.nsiv2.soap

import org.specs2.*

import jakarta.xml.bind.JAXBElement
import net.nordu.namespaces._2013._12.gnsbod.{ConnectionTraceType, ConnectionType}
import nl.surfnet.nsiv2.messages.*
import nl.surfnet.nsiv2.utils.*
import org.ogf.schemas.nsi._2013._12.framework.headers.SessionSecurityAttrType
import scala.jdk.CollectionConverters.*
import scala.util.Failure
import scala.util.Success
import org.apache.pekko.util.ByteString

@org.junit.runner.RunWith(classOf[runner.JUnitRunner])
class NsiSoapConversionsSpec extends mutable.Specification:
  import NsiSoapConversions.{given, *}

  val DefaultAckHeaders: NsiHeaders = NsiHeaders(
    CorrelationId.random(),
    "requesterNSA",
    "providerNSA",
    None,
    NsiHeaders.RequesterProtocolVersion
  )

  val providerOperationToStringConversion
      : Conversion[NsiProviderMessage[NsiProviderOperation], String] =
    NsiProviderMessageToDocument[NsiProviderOperation](None)
      .andThen(NsiXmlDocumentConversion)
      .andThen(ByteArrayToString)

  val requestOperationToStringConversion
      : Conversion[NsiRequesterMessage[NsiRequesterOperation], String] =
    NsiRequesterMessageToDocument[NsiRequesterOperation](None)
      .andThen(NsiXmlDocumentConversion)
      .andThen(ByteArrayToString)

  val requestAckToStringConversion: Conversion[NsiRequesterMessage[NsiAcknowledgement], String] =
    NsiRequesterMessageToDocument[NsiAcknowledgement](Some(DefaultAckHeaders))
      .andThen(NsiXmlDocumentConversion)
      .andThen(ByteArrayToString)

  "NSI requester operation to string" should {

    "parse reserveFailed operation" in {
      val reserveFailed =
        <SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">
          <SOAP-ENV:Header>
            <ns7:nsiHeader xmlns:ns2="http://schemas.ogf.org/nsi/2013/12/connection/types" xmlns:ns3="urn:oasis:names:tc:SAML:2.0:assertion" xmlns:ns4="http://www.w3.org/2001/04/xmlenc#" xmlns:ns5="http://www.w3.org/2000/09/xmldsig#" xmlns:ns6="http://schemas.ogf.org/nsi/2013/12/framework/types" xmlns:ns7="http://schemas.ogf.org/nsi/2013/12/framework/headers">
              <protocolVersion>application/vnd.ogf.nsi.cs.v2.requester+soap</protocolVersion>
              <correlationId>urn:uuid:fc15890f-3118-442f-8482-da50a303689e</correlationId>
              <requesterNSA>urn:ogf:network:nsa:surfnet-nsi-safnari</requesterNSA>
              <providerNSA>urn:ogf:network:netherlight.net:2013:nsa:bod</providerNSA>
            </ns7:nsiHeader>
          </SOAP-ENV:Header>
          <SOAP-ENV:Body>
            <ns2:reserveFailed xmlns:ns2="http://schemas.ogf.org/nsi/2013/12/connection/types" xmlns:ns3="urn:oasis:names:tc:SAML:2.0:assertion" xmlns:ns4="http://www.w3.org/2001/04/xmlenc#" xmlns:ns5="http://www.w3.org/2000/09/xmldsig#" xmlns:ns6="http://schemas.ogf.org/nsi/2013/12/framework/t ypes" xmlns:ns7="http://schemas.ogf.org/nsi/2013/12/framework/headers">
              <connectionId>3cb4b457-9c50-4285-bd47-f5d93f484dee</connectionId>
              <connectionStates>
                <reservationState>ReserveFailed</reservationState>
                <provisionState>Released</provisionState>
                <lifecycleState>Created</lifecycleState>
                <dataPlaneStatus>
                  <active>false</active>
                  <version>0</version>
                  <versionConsistent>true</versionConsistent>
                </dataPlaneStatus>
              </connectionStates>
              <serviceException>
                <nsaId>urn:ogf:network:netherlight.net:2013:nsa:bod</nsaId>
                <connectionId>3cb4b457-9c50-4285-bd47-f5d93f484dee</connectionId>
                <errorId>00200</errorId>
                <text>The VlanID specified in the source parameters overlaps with an existing service.</text>
              </serviceException>
            </ns2:reserveFailed>
          </SOAP-ENV:Body>
        </SOAP-ENV:Envelope>.toString

      val requesterMessage = requestOperationToStringConversion.invert(reserveFailed)

      requesterMessage must beSuccessfulTry
    }

    "parse dataPlaneStateChange operation" in {
      val message =
        <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
          <soap:Header>
            <ns6:nsiHeader xmlns:ns8="http://schemas.ogf.org/nsi/2013/12/services/point2point" xmlns:ns7="http://schemas.ogf.org/nsi/2013/12/framework/types" xmlns:ns6="http://schemas.ogf.org/nsi/2013/12/framework/headers" xmlns:ns5="http://schemas.ogf.org/nsi/2013/12/connection/types" xmlns:ns4="http://www.w3.org/2000/09/xmldsig#" xmlns:ns3="http://www.w3.org/2001/04/xmlenc#" xmlns:ns2="urn:oasis:names:tc:SAML:2.0:assertion">
              <protocolVersion>application/vnd.ogf.nsi.cs.v2.provider+soap</protocolVersion>
              <correlationId>urn:uuid:88bbe366-4af7-40bf-8edb-2ad9a980f402</correlationId>
              <requesterNSA>urn:ogf:network:nsa:surfnet-nsi-safnari</requesterNSA>
              <providerNSA>urn:ogf:network:nsa:es.net</providerNSA>
            </ns6:nsiHeader>
          </soap:Header>
          <soap:Body>
            <ns5:dataPlaneStateChange xmlns:ns2="urn:oasis:names:tc:SAML:2.0:assertion" xmlns:ns3="http://www.w3.org/2001/04/xmlenc#" xmlns:ns4="http://www.w3.org/2000/09/xmldsig#" xmlns:ns5="http://schemas.ogf.org/nsi/2013/12/connection/types" xmlns:ns6="http://schemas.ogf.org/nsi/2013/12/framework/headers" xmlns:ns7="http://schemas.ogf.org/nsi/2013/12/framework/types" xmlns:ns8="http://schemas.ogf.org/nsi/2013/12/services/point2point">
              <connectionId>2ac54e1f-3ce6-44af-9356-b0d4e31f3c42</connectionId>
              <notificationId>1</notificationId>
              <timeStamp>2013-09-26T06:19:49.311-07:00</timeStamp>
              <dataPlaneStatus>
                <active>true</active>
                <version>0</version>
                <versionConsistent>true</versionConsistent>
              </dataPlaneStatus>
            </ns5:dataPlaneStateChange>
          </soap:Body>
        </soap:Envelope>.toString

      val requesterMessage = requestOperationToStringConversion.invert(message)

      requesterMessage must beSuccessfulTry
    }

    "parse terminateConfirmed operation" in {
      val message =
        <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
          <soap:Header>
            <ns7:nsiHeader xmlns:ns8="http://schemas.ogf.org/nsi/2013/12/services/point2point" xmlns:ns7="http://schemas.ogf.org/nsi/2013/12/framework/headers" xmlns:ns6="http://schemas.ogf.org/nsi/2013/12/framework/types" xmlns:ns5="http://schemas.ogf.org/nsi/2013/12/connection/types" xmlns:ns4="http://www.w3.org/2000/09/xmldsig#" xmlns:ns3="http://www.w3.org/2001/04/xmlenc#" xmlns:ns2="urn:oasis:names:tc:SAML:2.0:assertion">
            <protocolVersion>application/vdn.ogf.nsi.cs.v2.provider+soap</protocolVersion>
            <correlationId>urn:uuid:65697f03-ec46-470d-8c4a-c28739cc863e</correlationId>
            <requesterNSA>urn:ogf:network:es.net:2013:nsa:nsi-aggr-west</requesterNSA>
            <providerNSA>urn:ogf:network:manlan.internet2.edu</providerNSA>
            <replyTo>https://oscars.bldc.manlan.internet2.edu:8500//ConnectionService</replyTo>
          </ns7:nsiHeader>
        </soap:Header>
        <soap:Body>
          <ns5:terminateConfirmed xmlns:ns2="urn:oasis:names:tc:SAML:2.0:assertion" xmlns:ns3="http://www.w3.org/2001/04/xmlenc#" xmlns:ns4="http://www.w3.org/2000/09/xmldsig#" xmlns:ns5="http://schemas.ogf.org/nsi/2013/12/connection/types" xmlns:ns6="http://schemas.ogf.org/nsi/2013/12/framework/types" xmlns:ns7="http://schemas.ogf.org/nsi/2013/12/framework/headers" xmlns:ns8="http://schemas.ogf.org/nsi/2013/12/services/point2point">
            <connectionId>urn:uuid:a46f19fa-fd4d-4bc5-ad68-390604bcf6fa</connectionId>
          </ns5:terminateConfirmed>
        </soap:Body>
      </soap:Envelope>.toString

      val requesterMessage = requestOperationToStringConversion.invert(message)

      requesterMessage must beSuccessfulTry
    }

    "parse SOAP fault without serviceException" in {
      val soapFault =
        <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
          <soap:Body>
            <soap:Fault>
              <faultcode>soap:Server</faultcode>
              <faultstring>Fault occurred while processing.</faultstring>
            </soap:Fault>
          </soap:Body>
        </soap:Envelope>.toString

      val requesterMessage = requestOperationToStringConversion.invert(soapFault)

      requesterMessage must beLike {
        case Failure(
              ErrorMessage(
                "SOAP fault without {http://schemas.ogf.org/nsi/2013/12/connection/types}serviceException. Fault string: Fault occurred while processing."
              )
            ) =>
          ok
      }
    }

    "parse serviceException" in {
      val soapFault =
        <S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/">
          <S:Header>
            <head:nsiHeader xmlns:head="http://schemas.ogf.org/nsi/2013/12/framework/headers">
              <protocolVersion>application/vnd.ogf.nsi.cs.v2.requester+soap</protocolVersion>
              <correlationId>urn:uuid:5c716e15-c17c-481e-885d-c9a5c06e0436</correlationId>
              <requesterNSA>urn:ogf:network:nsa:surfnet-nsi-requester</requesterNSA>
              <providerNSA>urn:ogf:network:nsa:surfnet.nl</providerNSA>
            </head:nsiHeader>
          </S:Header>
          <S:Body>
            <S:Fault>
              <faultcode>S:Server</faultcode>
              <faultstring>This operation is not supported by this provider</faultstring>
              <detail>
                <ns5:serviceException xmlns:ns7="http://schemas.ogf.org/nsi/2013/12/framework/headers" xmlns:ns6="http://schemas.ogf.org/nsi/2013/12/framework/types" xmlns:ns4="urn:oasis:names:tc:SAML:2.0:assertion" xmlns:ns3="http://www.w3.org/2000/09/xmldsig#" xmlns:ns2="http://www.w3.org/2001/04/xmlenc#" xmlns:ns5="http://schemas.ogf.org/nsi/2013/12/connection/types">
                  <nsaId>urn:ogf:network:netherlight.net:2013:nsa:bod</nsaId>
                  <errorId>103</errorId>
                  <text>Not Implemented</text>
                </ns5:serviceException>
              </detail>
            </S:Fault>
          </S:Body>
        </S:Envelope>.toString

      val requesterMessage = requestAckToStringConversion.invert(soapFault)

      requesterMessage must beLike {
        case Success(NsiRequesterMessage(headers, ServiceException(exception))) =>
          headers must_== NsiHeaders(
            CorrelationId.fromString("urn:uuid:5c716e15-c17c-481e-885d-c9a5c06e0436").get,
            "urn:ogf:network:nsa:surfnet-nsi-requester",
            "urn:ogf:network:nsa:surfnet.nl",
            None,
            NsiHeaders.RequesterProtocolVersion,
            Nil
          )
          exception.getErrorId() must_== "103"
      }

      requestAckToStringConversion(requesterMessage.get) must beLike { case Success(xml) =>
        xml must contain("<soapenv:Fault") and contain("<errorId>103</errorId>")
      }
    }

    "parse serviceException without NSI headers" in {
      val soapFault =
        <S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/">
          <S:Body>
            <S:Fault xmlns:ns4="http://www.w3.org/2003/05/soap-envelope">
              <faultcode>S:Server</faultcode>
              <faultstring>This operation is not supported by this provider</faultstring>
              <detail>
                <ns5:serviceException xmlns:ns7="http://schemas.ogf.org/nsi/2013/12/framework/headers" xmlns:ns6="http://schemas.ogf.org/nsi/2013/12/framework/types" xmlns:ns4="urn:oasis:names:tc:SAML:2.0:assertion" xmlns:ns3="http://www.w3.org/2000/09/xmldsig#" xmlns:ns2="http://www.w3.org/2001/04/xmlenc#" xmlns:ns5="http://schemas.ogf.org/nsi/2013/12/connection/types">
                  <nsaId>urn:ogf:network:netherlight.net:2013:nsa:bod</nsaId>
                  <errorId>103</errorId>
                  <text>Not Implemented</text>
                </ns5:serviceException>
              </detail>
            </S:Fault>
          </S:Body>
        </S:Envelope>.toString

      val requesterMessage = requestAckToStringConversion.invert(soapFault)

      requesterMessage must beLike { case Success(NsiRequesterMessage(headers, _)) =>
        headers must_== DefaultAckHeaders
      }
    }

  }

  "NSI provider operation to string" should {

    val reserveBody =
      <type:reserve>
        <description>A NSI reserve test</description>
        <criteria version="0">
          <schedule>
            <startTime>2013-07-24T16:50:00.000+02:00</startTime>
            <endTime>2013-07-24T17:00:00.000+02:00</endTime>
          </schedule>
          <serviceType>http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE</serviceType>
          <p2p:p2ps xmlns:p2p="http://schemas.ogf.org/nsi/2013/12/services/point2point">
            <capacity>100</capacity>
            <directionality>Bidirectional</directionality>
            <sourceSTP>
              <networkId>urn:ogf:network:stp:surfnet.nl</networkId>
              <localId>21</localId>
            </sourceSTP>
            <destSTP>
              <networkId>urn:ogf:network:stp:surfnet.nl</networkId>
              <localId>24</localId>
            </destSTP>
          </p2p:p2ps>
        </criteria>
      </type:reserve>

    "parse session security attributes in NSI header" in {
      val user = "johndoe"
      val token = "2b73b5c2-bb14-4606-9fc0-2d3d1ed026ab"
      val input =
        <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
            xmlns:head="http://schemas.ogf.org/nsi/2013/12/framework/headers"
            xmlns:type="http://schemas.ogf.org/nsi/2013/12/connection/types"
            xmlns:saml="urn:oasis:names:tc:SAML:2.0:assertion"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
          <soapenv:Header>
            <head:nsiHeader>
              <protocolVersion>application/vnd.ogf.nsi.cs.v2.provider+soap</protocolVersion>
              <correlationId>urn:uuid:5c716e15-c17c-481e-885d-c9a5c06e0436</correlationId>
              <requesterNSA>urn:ogf:network:nsa:surfnet-nsi-requester</requesterNSA>
              <providerNSA>urn:ogf:network:nsa:surfnet.nl</providerNSA>
              <replyTo>http://localhost:9000/reply</replyTo>
              <sessionSecurityAttr>
                <saml:Attribute Name="token">
                  <saml:AttributeValue xsi:type="xs:string">{token}</saml:AttributeValue>
                </saml:Attribute>
                <saml:Attribute Name="user">
                  <saml:AttributeValue xsi:type="xs:string">{user}</saml:AttributeValue>
                </saml:Attribute>
              </sessionSecurityAttr>
            </head:nsiHeader>
          </soapenv:Header>
          <soapenv:Body>{reserveBody}</soapenv:Body>
        </soapenv:Envelope>

      val Success(reserveMessage) =
        providerOperationToStringConversion.invert(input.toString): @unchecked

      reserveMessage must beLike { case NsiProviderMessage(headers: NsiHeaders, _) =>
        headers.sessionSecurityAttrs must contain(like[SessionSecurityAttrType] { case attrs =>
          attrs.getAttributeOrEncryptedAttribute() must haveSize(2)
        }).exactly(1)
      }

      val Success(output) = providerOperationToStringConversion.apply(reserveMessage): @unchecked

      output must contain(token)
      output must contain(user)
    }

    "parse connection trace in NSI headers" in {
      val input =
        <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
            xmlns:head="http://schemas.ogf.org/nsi/2013/12/framework/headers"
            xmlns:type="http://schemas.ogf.org/nsi/2013/12/connection/types"
            xmlns:gns="http://nordu.net/namespaces/2013/12/gnsbod">
          <soapenv:Header>
            <head:nsiHeader>
              <protocolVersion>application/vnd.ogf.nsi.cs.v2.provider+soap</protocolVersion>
              <correlationId>urn:uuid:5c716e15-c17c-481e-885d-c9a5c06e0436</correlationId>
              <requesterNSA>urn:ogf:network:nsa:surfnet-nsi-requester</requesterNSA>
              <providerNSA>urn:ogf:network:nsa:surfnet.nl</providerNSA>
              <replyTo>http://localhost:9000/reply</replyTo>
              <gns:ConnectionTrace>
                <Connection index="0">urn:ogf:network:surfnet.nl:1990:nsa:nsi-requester:noId</Connection>
                <Connection index="1">urn:ogf:network:es.net:2001:nsa:nsi-requester:1234567890</Connection>
              </gns:ConnectionTrace>
            </head:nsiHeader>
          </soapenv:Header>
          <soapenv:Body>{reserveBody}</soapenv:Body>
        </soapenv:Envelope>

      val Success(reserveMessage) =
        providerOperationToStringConversion.invert(input.toString): @unchecked

      reserveMessage must beLike { case NsiProviderMessage(headers: NsiHeaders, _) =>
        headers.any.elements must haveSize(1)
        headers.any.elements(0) must beAnInstanceOf[JAXBElement[_]]
        headers.any
          .elements(0)
          .asInstanceOf[JAXBElement[AnyRef]]
          .getValue must beAnInstanceOf[ConnectionTraceType]
        headers.any
          .elements(0)
          .asInstanceOf[JAXBElement[ConnectionTraceType]]
          .getValue
          .getConnection
          .asScala must contain(
          equalTo(
            new ConnectionType()
              .withValue("urn:ogf:network:es.net:2001:nsa:nsi-requester:1234567890")
              .withIndex(1)
          )
        )
      }

      val Success(output) = providerOperationToStringConversion.apply(reserveMessage): @unchecked

      output must contain(
        <Connection index="1">urn:ogf:network:es.net:2001:nsa:nsi-requester:1234567890</Connection>.toString
      )
    }
  }

  "DOM to byte array conversion" should {
    "validate and parse byte array" in {
      val input =
        <soapenv:Envelope
            xmlns:type="http://schemas.ogf.org/nsi/2013/12/connection/types"
            xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
            xmlns:head="http://schemas.ogf.org/nsi/2013/12/framework/headers">
          <soapenv:Header>
              <head:nsiHeader>
                  <protocolVersion>application/vnd.ogf.nsi.cs.v2.provider+soap</protocolVersion>
                  <correlationId>urn:uuid:5c716e15-c17c-481e-885d-c9a5c06e0436</correlationId>
                  <requesterNSA>urn:ogf:network:nsa:surfnet-nsi-requester</requesterNSA>
                  <providerNSA>urn:ogf:network:nsa:surfnet.nl</providerNSA>
                  <replyTo>http://localhost:9000/reply</replyTo>
              </head:nsiHeader>
          </soapenv:Header>
          <soapenv:Body>
              <type:reserve>
                  <globalReservationId/>
                  <description>A NSI reserve test</description>
                  <criteria version="0">
                      <schedule>
                          <startTime>2013-07-24T16:50:00.000+02:00</startTime>
                          <endTime>2013-07-24T17:00:00.000+02:00</endTime>
                      </schedule>
                      <serviceType>http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE</serviceType>
                      <p2p:p2ps xmlns:p2p="http://schemas.ogf.org/nsi/2013/12/services/point2point">
                          <capacity>100</capacity>
                          <directionality>Bidirectional</directionality>
                          <sourceSTP>
                              <networkId>urn:ogf:network:stp:surfnet.nl</networkId>
                              <localId>21</localId>
                          </sourceSTP>
                          <destSTP>
                              <networkId>urn:ogf:network:stp:surfnet.nl</networkId>
                              <localId>24</localId>
                          </destSTP>
                      </p2p:p2ps>
                  </criteria>
              </type:reserve>
          </soapenv:Body>
        </soapenv:Envelope>.toString

      val Success(dom) = NsiXmlDocumentConversion.invert(ByteString(input)): @unchecked
      dom.getDocumentElement().getLocalName() must beEqualTo("Envelope")

      val Success(output) = NsiXmlDocumentConversion(dom): @unchecked
      output.utf8String must contain("urn:uuid:5c716e15-c17c-481e-885d-c9a5c06e0436")
    }
  }

  "roundtrip conversion" should {
    "work for real example" in {
      val input =
        <soap:Envelope xmlns:ctypes="http://schemas.ogf.org/nsi/2013/12/connection/types" xmlns:header="http://schemas.ogf.org/nsi/2013/12/framework/headers" xmlns:ns2="urn:oasis:names:tc:SAML:2.0:assertion" xmlns:ns3="http://nordu.net/namespaces/2013/12/gnsbod" xmlns:p2psrv="http://schemas.ogf.org/nsi/2013/12/services/point2point" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
   <soap:Header>
      <header:nsiHeader>
         <protocolVersion>application/vnd.ogf.nsi.cs.v2.provider+soap</protocolVersion>
         <correlationId>urn:uuid:4cecd9a1-59fb-11e4-a2f2-7cd1c3dc3ba7</correlationId>
         <requesterNSA>urn:ogf:network:OpenNSA-CLI</requesterNSA>
         <providerNSA>urn:ogf:network:netherlight.net:2013:nsa:safnari</providerNSA>
         <replyTo>http://127.0.0.1:7000/NSI/services/RequesterService2</replyTo>
         <sessionSecurityAttr>
            <ns2:Attribute Name="token">
               <ns2:AttributeValue>621495cf-17c6-498c-870a-3c5b2d7ffd17</ns2:AttributeValue>
            </ns2:Attribute>
         </sessionSecurityAttr>
         <sessionSecurityAttr>
            <ns2:Attribute Name="user">
               <ns2:AttributeValue>hanst</ns2:AttributeValue>
            </ns2:Attribute>
         </sessionSecurityAttr>
         <ns3:ConnectionTrace>
            <Connection index="0">urn:ogf:network:OpenNSA-CLI:1</Connection>
         </ns3:ConnectionTrace>
      </header:nsiHeader>
   </soap:Header>
   <soap:Body>
      <ctypes:reserve>
         <description>surfnet.nl:1990:testbed:3878?vlan=2019 - surfnet.nl:1990:testbed:3921?vlan=2000</description>
         <criteria version="0">
            <schedule>
               <startTime>2014-10-22T16:16:30+00:00</startTime>
               <endTime>2014-10-22T16:46:30+00:00</endTime>
            </schedule>
            <serviceType>http://services.ogf.org/nsi/2013/12/descriptions/EVTS.A-GOLE</serviceType>
            <p2psrv:p2ps>
               <capacity>10</capacity>
               <directionality>Bidirectional</directionality>
               <sourceSTP>urn:ogf:network:surfnet.nl:1990:testbed:3878?vlan=2019</sourceSTP>
               <destSTP>urn:ogf:network:surfnet.nl:1990:testbed:3921?vlan=2000</destSTP>
            </p2psrv:p2ps>
         </criteria>
      </ctypes:reserve>
   </soap:Body>
</soap:Envelope>.toString

      val converter =
        NsiProviderMessageToDocument[NsiProviderOperation](None).andThen(NsiXmlDocumentConversion)
      val Success(doc) = converter.invert(ByteString(input)): @unchecked
      val Success(arr) = converter(doc): @unchecked

      converter.invert(arr) must beSuccessfulTry
    }
  }
end NsiSoapConversionsSpec
