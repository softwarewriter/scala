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

  def m4[F[_]: Applicative]()(implicit monadError: MonadError[F, Throwable]): F[Int] = {
    monadError.raiseError[Int](new NullPointerException("pelle"))
//    throw new NullPointerException("pelle")
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val io = for {
      i <- IO{42}
      j <- m4[IO]()
    }
    yield i + j
    io
      .attempt
      .flatMap(a => IO{println(a)})
//      .flatMap(a => IO{println(a)})
      .map(_ => ExitCode.Success)
  }

}
