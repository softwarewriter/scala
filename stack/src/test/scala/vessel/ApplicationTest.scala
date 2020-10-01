package vessel

import cats.effect.{Blocker, IO, Resource}
import io.circe.{Json, JsonObject}
import org.http4s.{EntityBody, Method, Request, Status, Uri, UrlForm}
import org.http4s.client.Client
import org.http4s.server.Server
import platform.implicits._
import io.circe.literal._
import org.scalatest.{Assertion, Succeeded}
import platform.Database
import org.http4s.client.dsl.io._
import org.scalatest.compatible.Assertion

import scala.concurrent.Future

class ApplicationTest extends platform.test.SharedResourceSpec {

  override type FixtureParam = (Client[IO])

  val testHTTPPort = 1338
  val testDBPort = 1433// platform.test.availablePort
// jdbc:sqlserver://localhost:1433;database=vessel
  override def resource: Resource[IO, FixtureParam] = {
    for {
      blocker    <- Blocker[IO]
      db         <- platform.test.Database[IO](testDBPort, "vessel", None)
      transactor <- platform.Database.transactor[IO](db, blocker)
      executionContext         <- platform.ExecutionContexts.cpuBoundExecutionContext[IO]("test-executionContext")
      application        <- Main.createApplication(Configuration(testHTTPPort, "0.0.0.0", Database.Config(s"jdbc:sqlserver://localhost:$testDBPort;databaseName=vessel", "com.microsoft.sqlserver.jdbc.SQLServerDriver", "sa", "Password123", None)))
      httpClient <- platform.HttpClient.rpc[IO](executionContext)
    } yield httpClient
  }

  "Testing vessel api" - {

    val existing = IMO("1")
    val nonExisting = IMO("4")
    val put = IMO("5")
    val putInconsistent1 = IMO("6")
    val putInconsistent2 = IMO("7")
    val delete = IMO("8")

    "Get existing test status" in {
      case httpClient =>
        assertHasStatus(httpClient, existing, Status.Ok)
    }

    "Get existing test vessel" in {
      case httpClient =>
        val vessel = Vessel(existing, "Titanic")
        assertHasVessel(httpClient, existing, vessel)
    }

    "Get non existing" in {
      case httpClient =>
        assertHasStatus(httpClient, nonExisting, Status.NotFound)
    }

    "Put" in {
      case httpClient =>
        val vessel1 = Vessel(put, "Synker aldri")
        val vessel2 = Vessel(put, "Synker sjelden")
        val isNotFound: IO[Assertion] = assertHasStatus(httpClient, put, Status.NotFound)
        val putOk: IO[Assertion] = httpClient
           .status(Request[IO](Method.PUT, uri(put))
              .withEntity(vessel1))
           .map(status => assert(status == Status.Ok))
        val hasVessel1: IO[Assertion] = assertHasVessel(httpClient, put, vessel1)
        val rePutOk: IO[Assertion] = httpClient
           .status(Request[IO](Method.PUT, uri(put))
              .withEntity(vessel2))
           .map(status => assert(status == Status.Ok))
        val hasVessel2: IO[Assertion] = assertHasVessel(httpClient, put, vessel2)

        for {
          isNotFoundResult <- isNotFound
          putOkResult <- putOk
          hasVessel1Result <- hasVessel1
          rePutOkResult <- rePutOk
          hasVessel2Result <- hasVessel2
        } yield allTrue(isNotFoundResult, putOkResult, hasVessel1Result, rePutOkResult, hasVessel2Result)
    }

    "Put inconsistent imo" in {
      case httpClient =>
        val vessel = Vessel(putInconsistent1, "Synker aldri")
        httpClient
           .status(Request[IO](Method.PUT, uri(putInconsistent2))
              .withEntity(vessel))
           .map(status => assert(status == Status.BadRequest))
    }

    "Delete" in {
      case httpClient =>
        val vessel = Vessel(delete, "Lars")
        val isNotFound = assertHasStatus(httpClient, delete, Status.NotFound)
        val putOk = httpClient
           .status(Request[IO](Method.PUT, uri(delete))
              .withEntity(vessel))
           .map(status => assert(status == Status.Ok))
        val isFound = assertHasStatus(httpClient, delete, Status.Ok)
        val deleteOk = httpClient
           .status(Request[IO](Method.DELETE, uri(delete)))
           .map(status => assert(status == Status.Ok))
        val secondDeleteNotOK = assertHasStatus(httpClient, delete, Status.NotFound)
        val isDeleted = httpClient
           .status(Request[IO](Method.DELETE, uri(delete)))
           .map(status => assert(status == Status.NotFound))

        for {
          isNotFoundResult <- isNotFound
          putOkResult <- putOk
          isFoundResult <- isFound
          deleteOkResult <- deleteOk
          secondDeleteNotOkResult <- secondDeleteNotOK
          isDeletedResult <- isDeleted
        } yield allTrue(isNotFoundResult, putOkResult, isFoundResult, deleteOkResult, secondDeleteNotOkResult, isDeletedResult)
    }

    "Search" in {
      case httpClient =>
        val expected = List(
          Vessel(IMO("2"), "Norge"),
          Vessel(IMO("3"), "Eidsvold")
        )
        val query = "o"
        httpClient
           .expect[Json](Request[IO](Method.GET, Uri.unsafeFromString("http://localhost:1338/vessel/search/" + query)))
           .map(json => assert(json == listOfVesselCodec.apply(expected)))
    }

  }

  def assertHasStatus(httpClient: Client[IO], imo: IMO, expectedStatus: Status): IO[Assertion] = {
    httpClient
    .status(Request[IO](Method.GET, uri(imo)))
    .map(status => assert(status == expectedStatus))
  }

  def assertHasVessel(httpClient: Client[IO], imo: IMO, expected: Vessel): IO[Assertion] = {
    httpClient
       .expect[Json](Request[IO](Method.GET, uri(imo)))
       .map(json => assert(json == vesselCodec.apply(expected)))
  }

  def uri(imo: IMO): Uri = {
    Uri.unsafeFromString(s"http://localhost:$testHTTPPort/vessel/" + imo.value)
  }

  def allTrue(assertions: Assertion*): Assertion =
  {
    assert(assertions.forall((assertion: Assertion) => assertion == Succeeded))
  }

}
