package no.jergan.scrapbook

import cats.data.{EitherT, OptionT}
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits.catsSyntaxApplicativeError
import cats.{Applicative, ApplicativeError, Monad}

/**
 * Nested monads.
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
      a <- EitherT[Option, Any, Int](Some(Right(42)))
      b <- EitherT[Option, Any, Int](Some(Right[Any, Int](43)))
    } yield {
      a + b
    }
    v.value match {
      case Some(Right(r)) => println(r)
      case _ => println("other")
    }
  }

  def semi(): Unit = {
    val v = EitherT[Option, Any, Int](Some(Right(42)))
      .semiflatMap(i => Some(i + 1))
      .value
    println(v)
  }

  def myTransformers(): Unit = {

    case class OT[F[_]: Monad, A](foa: F[Option[A]]) {

      def value: F[Option[A]] = foa

      def map[B](f: A => B): OT[F, B] =
        OT(Monad[F].map(foa) {
          case Some(a) => Some(f(a))
          case None => None
        })

      def flatMap[B](f: A => OT[F, B]): OT[F, B] = {
        OT(Monad[F].flatMap(foa) {
          case Some(a) => f(a).value
          case None => Applicative[F].pure(None)
        })
      }
    }

    case class ET[F[_]: Monad, A, B](fea: F[Either[A, B]]) {

      def value: F[Either[A, B]] = fea

      def map[C](f: B => C): ET[F, A, C] =
        ET(Monad[F].map(fea) {
          case Left(a) => Left[A, C](a)
          case Right(b) => Right[A, C](f(b))
        })

      def flatMap[C](f: B => ET[F, A, C]): ET[F, A, C] =
        ET(Monad[F].flatMap(fea) {
          case Left(a) => Applicative[F].pure(Left[A, C](a))
          case Right(b) => f(b).value
        })

    }

    val v = for {
      a <- EitherT[Option, Any, Int](Some(Right(42)))
      b <- EitherT[Option, Any, Int](Some(Right[Any, Int](43)))
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
    myTransformers()
    semi()
  }

}
