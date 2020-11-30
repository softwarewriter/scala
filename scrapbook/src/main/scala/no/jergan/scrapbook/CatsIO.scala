package no.jergan.scrapbook

import java.io._

import cats.Functor
import cats.effect.{ExitCode, IO, IOApp, Resource, Sync, Timer}
import cats.implicits._

/**
 * Test of cats.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object CatsIO extends IOApp {

  /*
  override def run(args: List[String]): IO[ExitCode] = {
    IO.pure(ExitCode.Error)
  }

   */

  override def run(args: List[String]): IO[ExitCode] = {
    println("Start of the world")
    val io1 = f1[IO]()
    val io2 = IO { println("io pelle2"); 1 }

    val io: IO[ExitCode] = io1
      .flatMap(a1 =>
        io2
          .flatMap(a2 => {
            println("hei: " + a2)
            IO.pure(a2 + 1)
          }
            .flatMap(a3 => {
              println("hei: " + a3)
              f2[IO]
                .map(a4 => ExitCode.Success)
            }
            )
          )
      )

    println("End of the world2")
    io
  }

  def f1[F[_]]()(implicit ev: Sync[F]) : F[Unit] = {
    println("f1")
    ev.delay{println("io f1")}
  }

  def f2[F[_]]()(implicit ev: Sync[F]) : F[Unit] = {
    println("f2")
    ev.delay{println("io f2")}
  }

}
