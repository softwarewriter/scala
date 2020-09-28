package vessel

import cats.effect.{Blocker, ExitCode, IO, IOApp}

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
