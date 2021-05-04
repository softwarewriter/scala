package no.jergan.scrapbook

import cats.{Applicative, ApplicativeError, Functor, MonadError}
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits.toFunctorOps
import cats.implicits.catsSyntaxApplicativeError

/**
 * Cats.
 */
object Monaderror extends IOApp {

  def m[F[_]: Applicative](s: String): F[Int] = {
    Applicative[F].pure(s.length)
  }

  def m2[F[_]: Sync](s: String)(implicit ev: ApplicativeError[F, Throwable]): F[Either[Throwable, Int]] = {
    m3(s).attempt
//    ApplicativeError[F, Throwable].attempt(m3(s))
  }

  def m3[F[_]: Sync](s: String): F[Int] = {
    Sync[F].delay {
      if (s.isEmpty) throw new NullPointerException("Was empty")
      s.length
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    /*
    implicit def myApplicativeError(implicit ev: Applicative[IO]): ApplicativeError[IO, Throwable] = new ApplicativeError[IO, Throwable] {

      override def raiseError[A](e: Throwable): IO[A] = IO {
        println(s"pelle1 ${e.getMessage}")
        throw e
      }

      override def handleErrorWith[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] = {
        println(s"pelle2")
        fa
      }

      override def pure[A](x: A): IO[A] = ev.pure(x)
      override def ap[A, B](ff: IO[A => B])(fa: IO[A]): IO[B] = ev.ap(ff)(fa)

    }

     */
//    val v = ApplicativeError[IO, Throwable]

    m2[IO]("")
      .flatMap(i => IO {println(s"Number was $i")})
      .map(_ => ExitCode.Success)
  }

}
