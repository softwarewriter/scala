package no.jergan.scrapbook

import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Sync, Timer}
import cats.effect.{IO, Sync}
import cats.effect.concurrent.Ref
import cats.syntax.all._
import scala.concurrent.ExecutionContext

/**
 * Cats.
 */
object CatsRef extends IOApp {

  class Store[F[_]: Timer](ref: Ref[F, Int])(implicit F: Sync[F]) {

    def get(): F[Int] = {
      ref.get
    }

    def set(i: Int): F[Unit] = {
      ref.set(i)
    }

  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      ref <- Ref.of[IO, Int](42)
      store  = new Store[IO](ref)
      v1 <- store.get()
      _ <- store.set(43)
      v2 <- store.get()
    } yield {
      println("v1: " + v1)
      println("v2: " + v2)
      ExitCode.Success
    }
/*
    for {
      ref <- Ref.of[IO, Int](42)
      store  = new Store[IO](ref)
      v1 <- store.get()
      w2  = new Worker[IO](2, ref)
      w3  = new Worker[IO](3, ref)
      _   <- List(
        w1.start,
        w2.start,
        w3.start
      ).parSequence.void
    } yield ExitCode.Success

 */
  }

}
