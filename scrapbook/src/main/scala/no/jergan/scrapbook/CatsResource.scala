package no.jergan.scrapbook

import cats.effect.{ExitCode, IO, IOApp, Resource}
import fs2.Stream

import scala.concurrent.duration.{DurationInt}

object CatsResource extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {

    def retry[A](thunk: IO[A]) = Stream.retry(thunk, 1.second, _ => 1.seconds, 3000).compile.lastOrError

    def acquire(): IO[String] = {
      retry(create())
    }

    def create(): IO[String] = {
      IO {
        println("create")
        throw new NullPointerException("pelle")
      }
//      IO("hei")
    }

    def release(s: String): IO[Unit] = {
      IO.unit
    }

    val resource = Resource.make[IO, String](acquire())(release)
    resource.use(s => IO(println("resource was " + s)))
      .map(_ => ExitCode.Success)
  }

}
