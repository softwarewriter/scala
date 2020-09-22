package no.jergan.scrapbook

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.client.{Client, JavaNetClientBuilder}

/**
  * What does this class do?
  *
  * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
  */
object HTTPClient extends IOApp {

  /*
 import org.http4s.client.blaze._
 import org.http4s.client._
 import scala.concurrent.ExecutionContext.global

 BlazeClientBuilder[IO](global).resource.use { client =>
    // use `client` here and return an `IO`.
    // the client will be acquired and shut down
    // automatically each time the `IO` is run.
    IO.unit
 }

   */

  import cats.effect.Blocker
  import java.util.concurrent._

  override def run(args: List[String]): IO[ExitCode] = {
    val blockingPool = Executors.newFixedThreadPool(5)
    val blocker = Blocker.liftExecutorService(blockingPool)
    val httpClient: Client[IO] = JavaNetClientBuilder[IO](blocker).create

    val request: IO[String] =
      httpClient.expect[String]("http://localhost:1337/echo/pelle")

    println(request.unsafeRunSync)
    IO(ExitCode.Success)
  }

}
