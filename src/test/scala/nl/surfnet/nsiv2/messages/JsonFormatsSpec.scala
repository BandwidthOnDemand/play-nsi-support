package nl.surfnet.nsiv2.messages

import java.net.URI
import java.time.Instant
import org.scalacheck.Arbitrary
import play.api.libs.json.*

@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class JsonFormatsSpec extends org.specs2.mutable.Specification with org.specs2.ScalaCheck:
  import Generators.given

  "Format[Instant]" should {
    "round-trip current time" in {
      val now = Instant.now()
      val json = Json.toJson(now)
      json must beAnInstanceOf[JsNumber]
      json.as[Long] must beEqualTo(now.toEpochMilli)

      val parsed = Json.fromJson[Instant](json)
      parsed must beLike { case JsSuccess(i, _) =>
        i.toEpochMilli must beEqualTo(now.toEpochMilli)
      }
    }

    "round-trip epoch" in {
      val epoch = Instant.EPOCH
      val json = Json.toJson(epoch)
      json must beEqualTo(JsNumber(0))

      Json.fromJson[Instant](json) must beLike { case JsSuccess(i, _) =>
        i must beEqualTo(Instant.EPOCH)
      }
    }

    "round-trip far future" in {
      val future = Instant.parse("2099-12-31T23:59:59Z")
      val json = Json.toJson(future)

      Json.fromJson[Instant](json) must beLike { case JsSuccess(i, _) =>
        i.toEpochMilli must beEqualTo(future.toEpochMilli)
      }
    }

    "round-trip arbitrary instants" in {
      prop { (instant: Instant) =>
        val json = Json.toJson(instant)
        val parsed = Json.fromJson[Instant](json)
        parsed must beLike { case JsSuccess(i, _) =>
          i.toEpochMilli must beEqualTo(instant.toEpochMilli)
        }
      }.set(minTestsOk = 50)
    }
  }

  "Format[URI]" should {
    "round-trip simple URI" in {
      val uri = URI.create("http://localhost:9000/path")
      val json = Json.toJson(uri)
      json must beEqualTo(JsString("http://localhost:9000/path"))

      Json.fromJson[URI](json) must beLike { case JsSuccess(u, _) =>
        u must beEqualTo(uri)
      }
    }

    "round-trip HTTPS URI with path and query" in {
      val uri = URI.create("https://example.com/nsi/v2?param=value")
      val json = Json.toJson(uri)
      Json.fromJson[URI](json) must beEqualTo(JsSuccess(uri))
    }

    "round-trip arbitrary URIs" in {
      prop { (uri: URI) =>
        val json = Json.toJson(uri)
        json must beAnInstanceOf[JsString]

        Json.fromJson[URI](json) must beLike { case JsSuccess(u, _) =>
          u must beEqualTo(uri)
        }
      }.set(minTestsOk = 20)
    }

    "fail on invalid URI" in {
      val json = JsString("not a valid uri with spaces")
      Json.fromJson[URI](json) must beLike { case JsError(_) => ok }
    }

    "fail on non-string JSON" in {
      Json.fromJson[URI](JsNumber(42)) must beLike { case JsError(_) => ok }
    }
  }

  "Format[CorrelationId]" should {
    "round-trip valid correlation ID" in {
      val corrId = CorrelationId.random()
      val json = Json.toJson(corrId)
      json must beAnInstanceOf[JsString]
      json.as[String] must startWith("urn:uuid:")

      Json.fromJson[CorrelationId](json) must beLike { case JsSuccess(c, _) =>
        c must beEqualTo(corrId)
      }
    }

    "round-trip arbitrary correlation IDs" in {
      prop { (corrId: CorrelationId) =>
        val json = Json.toJson(corrId)
        Json.fromJson[CorrelationId](json) must beLike { case JsSuccess(c, _) =>
          c must beEqualTo(corrId)
        }
      }.set(minTestsOk = 50)
    }

    "parse well-known correlation ID" in {
      val json = JsString("urn:uuid:5c716e15-c17c-481e-885d-c9a5c06e0436")
      Json.fromJson[CorrelationId](json) must beLike { case JsSuccess(c, _) =>
        c.toString must beEqualTo("urn:uuid:5c716e15-c17c-481e-885d-c9a5c06e0436")
      }
    }

    "fail on invalid correlation ID" in {
      val json = JsString("not-a-valid-correlation-id")
      Json.fromJson[CorrelationId](json) must beLike { case JsError(_) => ok }
    }

    "fail on missing urn:uuid: prefix" in {
      val json = JsString("5c716e15-c17c-481e-885d-c9a5c06e0436")
      Json.fromJson[CorrelationId](json) must beLike { case JsError(_) => ok }
    }

    "fail on non-string JSON" in {
      Json.fromJson[CorrelationId](JsNumber(42)) must beLike { case JsError(_) => ok }
    }
  }
end JsonFormatsSpec
