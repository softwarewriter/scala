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
//      blocker    <- Blocker[IO]
//      db         <- platform.test.Database[IO](testDBPort, "vessel", None)
//      transactor <- platform.Database.transactor[IO](db, blocker)
      executionContext         <- platform.ExecutionContexts.cpuBoundExecutionContext[IO]("test-executionContext")
      application        <- Main.createApplication(Configuration(testHTTPPort, "0.0.0.0", Database.Config(s"jdbc:sqlserver://localhost:$testDBPort;databaseName=vessel", "com.microsoft.sqlserver.jdbc.SQLServerDriver", "sa", "Password123", None)))
      httpClient <- platform.HttpClient.rpc[IO](executionContext)
    } yield httpClient
  }

  "Testing vessel api" - {

    val existing = "1"
    val nonExisting = "4"
    val uniqueForPut = "5"
    val uniqueForDelete = "6"

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
        val vessel1 = Vessel(nonExisting, "Synker aldri")
        val vessel2 = Vessel(nonExisting, "Synker sjelden")

        val doesNotExist: IO[Assertion] = assertHasStatus(httpClient, uniqueForPut, Status.NotFound)
        val insertVessel: IO[Assertion] = httpClient
           .status(Request[IO](Method.PUT, uri(uniqueForPut))
              .withEntity(vessel1))
           .map(status => assert(status == Status.Ok))
        val checkValueStoredStatus: IO[Assertion] = assertHasStatus(httpClient, uniqueForPut, Status.Ok)
        val checkValueStoredValue: IO[Assertion] = assertHasVessel(httpClient, uniqueForPut, vessel1)
        val updateVessel: IO[Assertion] = httpClient
           .status(Request[IO](Method.PUT, uri(uniqueForPut))
              .withEntity(vessel2))
           .map(status => assert(status == Status.Ok))
        val checkUpdatedStatus: IO[Assertion] = assertHasStatus(httpClient, uniqueForPut, Status.Ok)
        val checkUpdatedValue: IO[Assertion] = assertHasVessel(httpClient, uniqueForPut, vessel2)

        val result: IO[Assertion] = for {
          aDoesNotExist <- doesNotExist
          wasInserted <- insertVessel
          storedOk <- checkValueStoredStatus
        } yield {assert(List(aDoesNotExist, wasInserted, storedOk).forall((assertion: Assertion) => assertion == Succeeded)) }
        result
    }

    "Delete" in {
      case httpClient =>
        val vessel = Vessel(uniqueForDelete, "Lars")
        val isNotFound = assertHasStatus(httpClient, uniqueForDelete, Status.NotFound)
        val putOk = httpClient
           .status(Request[IO](Method.PUT, uri(uniqueForDelete))
              .withEntity(vessel))
           .map(status => assert(status == Status.Ok))
        val isFound = assertHasStatus(httpClient, uniqueForDelete, Status.Ok)
        val deleteOk = httpClient
           .status(Request[IO](Method.DELETE, uri(uniqueForDelete)))
           .map(status => assert(status == Status.Ok))
        val secondDeleteNotOK = assertHasStatus(httpClient, uniqueForDelete, Status.NotFound)
        val isDeleted = httpClient
           .status(Request[IO](Method.DELETE, uri(uniqueForDelete)))
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
          Vessel("2", "Norge"),
          Vessel("3", "Eidsvold")
        )
        val query = "o"
        httpClient
           .expect[Json](Request[IO](Method.GET, Uri.unsafeFromString("http://localhost:1338/vessel/search/" + query)))
           .map(json => assert(json == listOfVesselCodec.apply(expected)))
    }

  }

  def assertHasStatus(httpClient: Client[IO], imo: String, expectedStatus: Status): IO[Assertion] = {
    httpClient
    .status(Request[IO](Method.GET, uri(imo)))
    .map(status => assert(status == expectedStatus))
  }

  def assertHasVessel(httpClient: Client[IO], imo: String, expected: Vessel): IO[Assertion] = {
    httpClient
       .expect[Json](Request[IO](Method.GET, uri(imo)))
       .map(json => assert(json == vesselCodec.apply(expected)))
  }

  def uri(imo: String): Uri = {
    Uri.unsafeFromString(s"http://localhost:$testHTTPPort/vessel/" + imo)
  }

  def allTrue(assertions: Assertion*): Assertion =
  {
    assert(assertions.forall((assertion: Assertion) => assertion == Succeeded))
  }

  /*
  def assertAll(assertions: IO[Assertion]*): IO[Assertion] = {

    IO.eval(assert  (for {assertion <- assertions} yield assertion)
    val result: Int = for {assertion <- assertions} yield assertion

    val results: List[Assertion] = ???
    assert(results.forall((assertion: Assertion) => assertion == Succeeded)) }



    /*
    val result: IO[Assertion] = for {
      assertIsNotFound <- isNotFound
      assertPutOk <- putOk
      assertIsFound <- isFound
    } yield {assert(List(assertIsNotFound, assertPutOk, assertIsFound).forall((assertion: Assertion) => assertion == Succeeded)) }
    result

     */

  }
   */


}
