package no.jergan.scrapbook.scalawithcats

import cats.{Applicative}
import cats.data.{EitherT}

import scala.concurrent.Future

object Ex54 {

  type Response[A] = EitherT[Future, String, A]

  object Response {
//    def apply[A](a: A)(implicit ev: Applicative[Future]): Response[A] = EitherT[Future, String, A](ev.pure(Right(a)))
//    def apply[A](error: String)(implicit ev: Applicative[Future]): Response[A] = EitherT[Future, String, A](ev.pure(Left(error)))
  }

  def getPowerLevel(autobot: String)(implicit ev: Applicative[Future]): Response[Int] = {
    val powerLevels = Map(
      "Jazz"      -> 6,
      "Bumblebee" -> 8,
      "Hot Rod"   -> 10
    )
    powerLevels.get(autobot) match {
      case Some(p) => EitherT.right(Applicative[Future].pure(p))
      case None => EitherT.left(Applicative[Future].pure(s"Missing autobot $autobot"))
    }
  }
/*
  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      p1 <- getPowerLevel(ally1)
      p2 <- getPowerLevel(ally2)
    } yield {

    }

 */



  def main(args: Array[String]): Unit = {

    println("pelle")
  }

}
