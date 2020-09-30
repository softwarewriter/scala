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
import vessel.SimpleVesselService.put

import scala.concurrent.Future

class ApplicationTest extends platform.test.SharedResourceSpec {

  override type FixtureParam = (Client[IO])

  val dbPort = platform.test.availablePort

  override def resource: Resource[IO, FixtureParam] = {
    for {
      blocker    <- Blocker[IO]
//      db         <- platform.test.Database[IO](dbPort, "vessel", None)
//      transactor <- platform.Database.transactor[IO](db, blocker)
      executionContext         <- platform.ExecutionContexts.cpuBoundExecutionContext[IO]("test-executionContext")
      application        <- Main.createApplication(Configuration(1338, "0.0.0.0", Database.Config("url", "driverClassName", "sa", "Password1234", None)))
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
        val a1 = assertHasStatus(httpClient, uniqueForPut, Status.NotFound)
        val a2 = httpClient
           .status(Request[IO](Method.PUT, uri(uniqueForPut))
              .withEntity(vessel1))
           .map(status => assert(status == Status.Ok))
        val a3 = assertHasStatus(httpClient, uniqueForPut, Status.Ok)
        val a4 = assertHasVessel(httpClient, uniqueForPut, vessel1)
        val a5 = httpClient
           .status(Request[IO](Method.PUT, uri(uniqueForPut))
              .withEntity(vessel2))
           .map(status => assert(status == Status.Ok))
        val a6 = assertHasStatus(httpClient, uniqueForPut, Status.Ok)
        val a7 = assertHasVessel(httpClient, uniqueForPut, vessel2)
        combine(a1, a2, a3, a4, a5, a6, a7)
    }

    "Delete" in {
      case httpClient =>
        val vessel = Vessel(nonExisting, "Lars")
        val a1 = assertHasStatus(httpClient, uniqueForDelete, Status.NotFound)
        val a2 = httpClient
           .status(Request[IO](Method.PUT, uri(uniqueForDelete))
              .withEntity(vessel))
           .map(status => assert(status == Status.Ok))
        val a3 = assertHasStatus(httpClient, uniqueForDelete, Status.Ok)
        val a4 = httpClient
           .status(Request[IO](Method.DELETE, uri(uniqueForDelete)))
           .map(status => assert(status == Status.Ok))
        val a5 = assertHasStatus(httpClient, uniqueForDelete, Status.NotFound)
        val a6 = httpClient
           .status(Request[IO](Method.DELETE, uri(uniqueForDelete)))
           .map(status => assert(status == Status.NotFound))
        combine(a1, a2, a3, a4, a5, a6)
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
    Uri.unsafeFromString("http://localhost:1338/vessel/" + imo)
  }

  def combine(futures: Future[Assertion]*): Future[Assertion] = {
//    val v1: Future[Assertion] = Future.foldLeft(futures)(assert(true))((r: Assertion, t: Assertion) => assert(r == Succeeded && t == Succeeded))
//    v1

    val v1: Future[List[Assertion]] = Future.sequence(futures.toList)
    def f: Assertion => Boolean = (a: Assertion) => a == Succeeded
    val v2: Future[Assertion] = v1.map(listOfAssertions => assert(listOfAssertions.forall(f)))
    v2

  }

}
