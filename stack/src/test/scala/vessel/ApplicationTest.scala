package vessel

import cats.effect.{Blocker, IO, Resource}
import io.circe.Json
import org.http4s.{EntityBody, Method, Request, Status, Uri, UrlForm}
import org.http4s.client.Client
import org.http4s.server.Server
import platform.implicits._
import io.circe.literal._
import org.scalatest.Assertion
import platform.Database
import org.http4s.client.dsl.io._

class ApplicationTest extends platform.test.SharedResourceSpec {

  override type FixtureParam = (Server[IO], Client[IO])

  val dbPort = platform.test.availablePort

  override def resource: Resource[IO, FixtureParam] = {
    for {
      blocker    <- Blocker[IO]
//      db         <- platform.test.Database[IO](dbPort, "vessel", None)
//      transactor <- platform.Database.transactor[IO](db, blocker)
      executionContext         <- platform.ExecutionContexts.cpuBoundExecutionContext[IO]("test-ec")
      application        <- Main.createApplication(Configuration(1338, "0.0.0.0", SimpleVesselService, Database.Config("url", "driverClassName", "sa", "Password1234", None)))
      httpClient <- platform.HttpClient.rpc[IO](executionContext)
    } yield application -> httpClient
  }

  "Testing vessel api" - {

    val existing = "1"
    val nonExisting = "4"

    "Get existing" in {
      case (application, httpClient) =>
        assertImoHasStatus(httpClient, existing, Status.Ok)
    }

    "Get non existing" in {
      case (application, httpClient) =>
        assertImoHasStatus(httpClient, nonExisting, Status.NotFound)
    }

    "Put" in {
      case (application, httpClient) =>
        val nonExisting = "5"
        assertImoHasStatus(httpClient, nonExisting, Status.NotFound)

        httpClient
           .status(Request[IO](Method.PUT, uri(nonExisting))
              .withEntity(Vessel(nonExisting, "Lars")))
           .map(status => assert(status == Status.Ok))

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
    }

    "Using correct url" in {
      case (app, httpClient) =>
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

}
