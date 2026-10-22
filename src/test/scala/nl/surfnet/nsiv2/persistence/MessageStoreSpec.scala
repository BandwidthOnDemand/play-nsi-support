package nl.surfnet.nsiv2.persistence

import nl.surfnet.nsiv2.messages.*
import nl.surfnet.nsiv2.soap.Conversion
import org.scalacheck.Gen
import org.specs2.specification.BeforeAfterAll
import play.api.db.Database
import play.api.db.Databases
import scala.io.Source
import scala.util.Try
import scala.util.Using
import MessageData.{formatJson, parseJson}

/** Runs [[MessageStoreSpecification]] against PostgreSQL; see the README for the database. */
@org.junit.runner.RunWith(classOf[org.specs2.runner.JUnitRunner])
class MessageStoreSpec extends MessageStoreSpecification with BeforeAfterAll:
  import Generators.given

  override type Message = NsiProviderMessage[NsiProviderOperation]
  override given MessageConversion: Conversion[Message, MessageData] =
    Conversion.build[Message, MessageData] { message =>
      Try(MessageData(Some(message.headers.correlationId), "FromRequester", formatJson(message)))
    } { data => parseJson[Message](data.content) }
  override given MessageGenerator: Gen[Message] =
    Gen.resultOf(NsiProviderMessage.apply[NsiProviderOperation] _)

  override lazy val database: Database = Databases(
    "org.postgresql.Driver",
    "jdbc:postgresql://localhost/play-nsi-support-test",
    config = Map("username" -> "play-nsi-support-user", "password" -> "password")
  )

  override def beforeAll(): Unit =
    val schema = Using.resource(Source.fromResource("message-store-schema.sql"))(_.mkString)
    database.withConnection(_.createStatement().execute(schema))

  override def afterAll(): Unit = database.shutdown()
end MessageStoreSpec
