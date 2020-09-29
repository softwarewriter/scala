package vessel

import cats.effect.{Blocker, IO, Resource}
import io.circe.Json
import org.http4s.{Method, Request, Uri}
import org.http4s.client.Client
import org.http4s.server.Server
import platform.implicits._
import io.circe.literal._

class AppTest { /*extends platform.test.SharedResourceSpec {
  override type FixtureParam = (Server[IO], Client[IO])
  val dbPort = platform.test.availablePort

  override def resource: Resource[IO, FixtureParam] = {
    for {
      blocker    <- Blocker[IO]
      db         <- platform.test.Database[IO](dbPort, "vessel", None)
      transactor <- platform.Database.transactor[IO](db, blocker)
      ec         <- platform.ExecutionContexts.cpuBoundExecutionContext[IO]("test-ec")
      app        <- Main.application(Main.Config(1338, "0.0.0.0"))
      httpClient <- platform.HttpClient.rpc[IO](ec)
    } yield app -> httpClient
  }

  "Testing Health Endpoint" - {

    "Using correct url" in {
      case (app, httpClient) =>
        val expected = json"""{"version":"NOT AN TAGGED VERSION","versionUrl":"NOT AN TAGGED VERSION"}"""
        httpClient
          .expect[Json](Request[IO](Method.GET, Uri.unsafeFromString("http://localhost:1338/health")))
          .map(json => assert(json == expected))
    }

    "Using wrong url" in {
      case (app, httpClient) =>
        httpClient
          .successful(Request[IO](Method.GET, Uri.unsafeFromString("http://localhost:1338/healthh")))
          .map(success => assert(success == false))
    }
  }
  */
}
