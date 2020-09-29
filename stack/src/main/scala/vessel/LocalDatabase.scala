package vessel

import cats.effect.{Blocker, ExitCode, IO, IOApp}

/**
 * What does this class do?
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object LocalDatabase extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    val app = for {
      blocker  <- Blocker[IO]
      dbServer <- platform.test.Database[IO](1433, "vessel", None)
      _        <- platform.Database.transactor[IO](dbServer, blocker)
    } yield dbServer

    app.use(_ => IO.never)
  }
}
