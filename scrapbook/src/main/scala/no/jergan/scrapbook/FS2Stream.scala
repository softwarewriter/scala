package no.jergan.scrapbook

import cats.effect.{Concurrent, ExitCode, IO, IOApp, Sync, Timer}
import fs2.Stream

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{Duration, DurationInt}

object FS2Stream extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    Stream
      .repeatEval(timeout(probe[IO]()))
      .evalTap(
        b =>
          IO {
            println(s"value: $b at ${System.currentTimeMillis()}")
          }
      )
      .metered(5.seconds)
      .map(_ => ExitCode.Success)
      .compile
      .lastOrError
  }

  def timeout[F[_]: Sync: Concurrent: Timer](fb: F[Boolean]): F[Boolean] = {
    Concurrent.timeout(fb, Duration(1, TimeUnit.SECONDS))
  }

  def probe[F[_]: Sync](): F[Boolean] = {
    Sync[F].delay {
      println("Sleeping")
      Thread.sleep(10000)
      println("Has slept")
      true
    }
  }

}
