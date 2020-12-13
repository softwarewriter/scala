package no.jergan.scrapbook

import java.io._

import cats.Functor
import cats.effect.{ExitCode, IO, IOApp, Resource, Sync, Timer}
import cats.implicits._

/**
 * Cats.
 */
object CatsIO extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    println("Start of the world")
    val io1 = f1[IO]()
    val io2 = f2[IO]()

    val io = io1
      .flatMap(_ => {
        val x = 10 * 100 // this is pure, so idiomatic acceptable to have outside of IO.
        IO { println("bind") }
          .flatMap(_ => io2)
      })
    println("End of the world2")

    io.map(_ => ExitCode.Success)
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
