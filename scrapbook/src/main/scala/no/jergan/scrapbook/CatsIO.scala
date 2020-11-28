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

  override def run(args: List[String]): IO[ExitCode] = {
    IO.pure(ExitCode.Error)
  }

  /*
override def run(args: List[String]): IO[ExitCode] = {
   println("Start of the world")
   val io1 = f1
   val io2 = IO { println("io pelle2"); 42 }

   val io: IO[ExitCode] = io1
     .flatMap(a1 =>
        io2
          .flatMap(a2 => {
            println("hei: " + a2)
             f(IO.pure(42))
          }
            .flatMap(a3 =>
               f2[IO]
                 .map(a4 => ExitCode.Success)
            )
          )
     )

   println("End of the world2")
   io

   /*
    io1
      .flatMap(a => io2)
      .flatMap(_ => {
         println("hei")
         f(IO.pure(42))
      })
      .flatMap(_ => io3)
      .map(_ => ExitCode.Success)
    */
}
// implicit ev: Sync[F]

def f1[F[_]]()(implicit ev: Sync[F]) : F[Unit] = {
   println("f1")
   v.map(i => println("io f: " + i))
}

def f[F[_]](v: F[Int])(implicit ev: Functor[F]) : F[Unit] = {
   println("f")
   v.map(i => println("io f: " + i))
}

def f2[F[_]]()(implicit ev: Sync[F]) : F[Unit] = {
   println("f2")
//      ev.delay{ println("IO f2")}
   Sync[F](ev).delay{ println("IO f2")}
}

   */

}
