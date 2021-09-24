package no.jergan.scrapbook

import cats.{Applicative, Functor}
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp, Resource, Sync, Timer}
import fs2.{RaiseThrowable, Stream}

import scala.concurrent.duration.DurationInt

/**
 * What does this class do?
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object ResourceAndRetry extends IOApp {

  var number = 0

  def nextNumber(): Int = {
    number += 1
    number
  }

  case class MyResource(number: Int) {
  }

  override def run(args: List[String]): IO[ExitCode] = {
    createResource[IO]()
      .use(mr => IO(println(mr.number)))
      .map(_ => ExitCode.Success)
  }

  def createResource[F[_]: Sync: Applicative: Timer: RaiseThrowable](): Resource[F, MyResource] = {

    def retry[A](thunk: F[A]): F[A] = Stream.retry(thunk, 1.second, _ => 1.seconds, 30).compile.lastOrError

    def acquire(): F[MyResource] = Sync[F].delay {

      val myResource = MyResource(nextNumber())
      println(s"created resource ${myResource.number}")
      if (myResource.number < 5) {
        throw new RuntimeException(s"failed creating resource ${myResource.number}")
      }
      myResource
    }

    def onError(): F[Unit] =
      //Sync[F].delay(println("failed, cleanup and trying again"))
      Sync[F].delay(throw new RuntimeException("failing on cleanup after failure"))

    def release(myResource: MyResource): F[Unit] = Sync[F].delay{println(s"cleanup after resource ${myResource.number}")}

    Resource.make(retry(acquire().onError(_ => onError())))(mr => release(mr))
  }

}
