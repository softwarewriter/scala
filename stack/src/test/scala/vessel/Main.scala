package vessel

import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource}
import io.circe.{Decoder, Encoder}
import org.http4s.server.Server
import platform.Database

/**
  * Entry point.
  *
  * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
  */
// Define your application as an resource

object main extends IOApp {

  implicit def entityEncoder[A: Encoder] = org.http4s.circe.jsonEncoderOf[IO, A]

  def createApplication(configuration: Configuration): Resource[IO, Server[IO]] = {

    //val ec: Resource[IO, ExecutionContext] = platform.ExecutionContexts.cpuBoundExecutionContext[IO]("main-ec")
    //val httpServer: Resource[IO, Server[IO]] = ec.flatMap(executionContext => platform.HttpServer[IO](config.port,config.bindAddress,executionContext, ???))

    // Http service.. f(x) : Request => Response
    // f(x): PartialFunction[Request, Response]
    // f(x): Request => Option[Response]
    // flatMappe the shit A => F[B]
    //PartialFunction[Request, Response] -> Request => Option[Response] -> Kleisli[Option, Request, Response] -> ||| type PartialKleisli[F[_], A, B] = Kleisli[OptionT[F, *], Request[F], Response[F]]

    val httpServer = for {
      executionContext <- platform.ExecutionContexts.cpuBoundExecutionContext[IO]("main-execution-context")
      blocker          <- Blocker[IO]
//         transactor <- platform.Database.transactor[IO](configuration.databaseConfiguration, blocker)
      httpServer <- Endpoints.create[IO](configuration, executionContext)
    } yield httpServer

    httpServer
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val vesselService = SimpleVesselService
    val configuration = Configuration(1337, "0.0.0.0", vesselService, Database.Config("url", "driverClassName", "sa", "Password1234", None))
    val application = createApplication(configuration)
    application.use(_ => IO.never)
  }

}
