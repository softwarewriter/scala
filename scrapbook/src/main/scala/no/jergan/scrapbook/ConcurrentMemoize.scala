package no.jergan.scrapbook

import cats.Applicative
import cats.effect.{Concurrent, ExitCode, IO, IOApp, Sync}
import cats.implicits.catsSyntaxFlatten

/**
 * Cats.
 */
object ConcurrentMemoize extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {

    val io1 = IO{
      println("calculates...")
      42
    }
    val mio = Concurrent.memoize(io1)
//    val fmio = mio.flatten
    val io2 = for {
      a <- mio
      b <- mio
      c  = Applicative[IO].map2(a, b){case (aa, bb) => aa + bb}
    } yield c
    io2.flatten.map(_ => ExitCode.Success)
  }

}
