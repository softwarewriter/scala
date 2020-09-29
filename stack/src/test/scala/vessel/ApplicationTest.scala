package vessel

import cats.effect.{Blocker, IO, Resource}
import io.circe.Json
import org.http4s.{Method, Request, Status, Uri}
import org.http4s.client.Client
import org.http4s.server.Server
import platform.implicits._
import io.circe.literal._
import org.scalatest.Assertion
import platform.Database

class ApplicationTest extends platform.test.SharedResourceSpec {

  override type FixtureParam = (Server[IO], Client[IO])

  val existing = "1"
  val nonExisting = "4"

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

    "Get" in {
      case (application, httpClient) =>
        assertImoHasStatus(httpClient, existing, Status.Ok)
        assertImoHasStatus(httpClient, nonExisting, Status.NotFound)
/*
        assert(hasImo(httpClient, existing))
        httpClient
           .status(Request[IO](Method.GET, uri(existing)))
           .map(status => assert(status == Status.Ok))

 */
        httpClient
           .status(Request[IO](Method.GET, uri(nonExisting)))
           .map(status => assert(status == Status.NotFound))
    }

    "Put" in {
      case (application, httpClient) =>
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

  def assertImoHasStatus(httpClient: Client[IO], imo: String, expectedStatus: Status): Assertion = {
    httpClient
    .status(Request[IO](Method.GET, uri(imo)))
    .map(status => assert(status == expectedStatus)).unsafeRunSync()
  }

  def uri(imo: String): Uri = {
    Uri.unsafeFromString("http://localhost:1338/vessel/" + imo)
  }

}
