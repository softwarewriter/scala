package vessel

import cats.effect.{ExitCode, IO, IOApp, Resource}
import org.http4s.HttpRoutes
import org.http4s.server.Server

import scala.concurrent.ExecutionContext

object Main extends IOApp {
  case class Config(port: Int, bindAddress: String)
  // Define your app as an resource
  def app(config: Config): Resource[IO, Server[IO]] = {

    //val ec: Resource[IO, ExecutionContext] = platform.ExecutionContexts.cpuBoundExecutionContext[IO]("main-ec")
    //val httpServer: Resource[IO, Server[IO]] = ec.flatMap(executionContext => platform.HttpServer[IO](config.port,config.bindAddress,executionContext, ???))

    val httpServer = for {
      ec         <- platform.ExecutionContexts.cpuBoundExecutionContext[IO]("main-ec")
      health     = platform.HttpServer.healthCheck[IO]()
      httpServer <- platform.HttpServer[IO](config.port, config.bindAddress, ec, health)
    } yield httpServer

    httpServer
  }

  override def run(args: List[String]): IO[ExitCode] = {
    // Read config
    // create App
    // Run the app
    app(Config(1337,"0.0.0.0")).use(_ => IO.never)
  }
}
