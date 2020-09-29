package vessel

import cats.effect.{Blocker, IO, Resource}
import io.circe.Json
import org.http4s.{EntityBody, Method, Request, Status, Uri, UrlForm}
import org.http4s.client.Client
import org.http4s.server.Server
import platform.implicits._
import io.circe.literal._
import org.scalatest.{Assertion, Succeeded}
import platform.Database
import org.http4s.client.dsl.io._

import scala.concurrent.Future

class ApplicationTest extends platform.test.SharedResourceSpec {

  override type FixtureParam = (Client[IO])

  val dbPort = platform.test.availablePort

  override def resource: Resource[IO, FixtureParam] = {
    for {
      blocker    <- Blocker[IO]
//      db         <- platform.test.Database[IO](dbPort, "vessel", None)
//      transactor <- platform.Database.transactor[IO](db, blocker)
      executionContext         <- platform.ExecutionContexts.cpuBoundExecutionContext[IO]("test-ec")
      application        <- Main.createApplication(Configuration(1338, "0.0.0.0", SimpleVesselService, Database.Config("url", "driverClassName", "sa", "Password1234", None)))
      httpClient <- platform.HttpClient.rpc[IO](executionContext)
    } yield httpClient
  }

  "Testing vessel api" - {

    "Get existing" in {
      case (httpClient) =>
        val existing = "1"
        assertImoHasStatus(httpClient, existing, Status.Ok)
    }

    "Get non existing" in {
      case (httpClient) =>
        val nonExisting = "4"
        assertImoHasStatus(httpClient, nonExisting, Status.NotFound)
    }

    "Put" in {
      case (httpClient) =>
        val nonExisting = "5"
        val v1: Future[Assertion] = assertImoHasStatus(httpClient, nonExisting, Status.NotFound)
        val v2: Future[Assertion] = httpClient
           .status(Request[IO](Method.PUT, uri(nonExisting))
              .withEntity(Vessel(nonExisting, "Lars")))
           .map(status => assert(status == Status.Ok))
        val v3: Future[Assertion] = assertImoHasStatus(httpClient, nonExisting, Status.Ok)

        //        v1 andThen v2
        combine(v1, v2, v3)

      /*

      val expected = json"""{"version":"NOT AN TAGGED VERSION","versionUrl":"NOT AN TAGGED VERSION"}"""
      httpClient
         .expect[Json](Request[IO](Method.GET, Uri.unsafeFromString("http://localhost:1338/health")))
         .map(json => assert(json == expected))



      httpClient
         .status(Request[IO](Method.GET, uri("4")))
         .map(status => assert(status == Status.Ok))
      httpClient
         .status(Request[IO](Method.GET, uri("4")))
         .map(status => assert(status == Status.NotFound))

       */
    }

    "Using correct url" in {
      case (httpClient) =>
        val expected = json"""{"version":"NOT AN TAGGED VERSION","versionUrl":"NOT AN TAGGED VERSION"}"""
        httpClient
           .expect[Json](Request[IO](Method.GET, Uri.unsafeFromString("http://localhost:1338/health")))
           .map(json => assert(json == expected))
    }

  }

  def assertImoHasStatus(httpClient: Client[IO], imo: String, expectedStatus: Status): IO[Assertion] = {
    httpClient
    .status(Request[IO](Method.GET, uri(imo)))
    .map(status => assert(status == expectedStatus))
  }

  def uri(imo: String): Uri = {
    Uri.unsafeFromString("http://localhost:1338/vessel/" + imo)
  }

  def combine(futures: Future[Assertion]*): Future[Assertion] = {

    val v1: Future[List[Assertion]] = Future.sequence(futures.toList)

    def f: Assertion => Boolean = (a: Assertion) => true
    val v2: Future[Assertion] = v1.map(listOfAssertions => listOfAssertions.forall(f))
    v2
  }

}
