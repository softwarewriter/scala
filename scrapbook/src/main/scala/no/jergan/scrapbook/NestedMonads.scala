package no.jergan.scrapbook

import cats.data.EitherT
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits.catsSyntaxApplicativeError
import cats.{Applicative, ApplicativeError}

/**
 * Cats.
 */
object NestedMonads {

  def simple(): Unit = {
    val v = for {
    a <- Right(42)
    b <- Right(43)
    } yield {
      a + b
    }
    v match {
      case Left(l) => println(l)
      case Right(r) => println(r)
    }
  }

  def complex(): Unit = {
    val v = for {
      aE <- Some(Right(42))
      bE <- Some(Right(43))
    } yield {
      for {
        a <- aE
        b <- bE
      }
      yield {
        a + b
      }
    }
    v match {
      case Some(Right(r)) => println(r)
      case _ => println("other")
    }
  }

  def complexWithEitherT(): Unit = {
    val v = for {
      a <- EitherT(Some(Right(42)): Option[Either[Any, Int]])
      b <- EitherT(Some(Right(43)): Option[Either[Any, Int]])
    } yield {
      a + b
    }
    v.value match {
      case Some(Right(r)) => println(r)
      case _ => println("other")
    }
  }

  def main (args: Array[String] ): Unit = {
    simple()
    complex()
    complexWithEitherT()
  }

}
