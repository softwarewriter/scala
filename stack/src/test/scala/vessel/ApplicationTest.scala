package vessel

import cats.effect.{Blocker, IO, Resource}
import io.circe.Json
import org.http4s.{Method, Request, Status, Uri}
import org.http4s.client.Client
import org.http4s.server.Server
import platform.implicits._
import io.circe.literal._
import platform.Database

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

    "Get" in {
      case (application, httpClient) =>
        httpClient
           .statusFromUri(Uri.unsafeFromString("http://localhost:1338/vessel"))
           .map(status => assert(status == Status.NotFound))
    }

  }

}
