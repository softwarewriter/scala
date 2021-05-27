package no.jergan.scrapbook

import cats.Functor
import cats.effect.{Concurrent, ContextShift, ExitCode, IO, IOApp, Sync, Timer}
import cats.implicits._

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object CatsBackground extends IOApp {

  trait Background[F[_]] { def schedule[A](
                                            fa: F[A],
                                            duration: FiniteDuration
                                          ): F[Unit]
  }
/*
  implicit def concurrentBackground[F[_]: ContextShift: Concurrent: Timer]: Background[F] = new Background[F] {
    def schedule[A]( fa: F[A],
                     duration: FiniteDuration
                   ): F[Unit] =
      (Timer[F].sleep(duration) *> fa).start.void
  }

 */

  override def run(args: List[String]): IO[ExitCode] = {
    val fa: IO[Int] = Sync[IO].pure(42)

    Functor
    val fb = fa.ensure(new NullPointerException("hei"))(_ > 44)

    fb
      .map(_ => ExitCode.Success)
  }

}
