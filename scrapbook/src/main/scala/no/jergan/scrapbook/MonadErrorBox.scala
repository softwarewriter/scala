package no.jergan.scrapbook

import cats.{Applicative, MonadError}

object MonadErrorBox {

  case class Box[A](a: A) {

    def map[B](f: A => B): Box[B] = Box(f(a))
    def flatMap[B](f: A => Box[B]): Box[B] = f(a)
  }

  object Box {

    implicit val applicative: Applicative[Box] = new Applicative[Box] {
      override def pure[A](x: A): Box[A] = Box(x)
      override def ap[A, B](ff: Box[A => B])(fa: Box[A]): Box[B] = ???
    }

    implicit val monadError: MonadError[Box, String] = new MonadError[Box, String] {

      override def flatMap[A, B](fa: Box[A])(f: A => Box[B]): Box[B] = ???
      override def tailRecM[A, B](a: A)(f: A => Box[Either[A, B]]): Box[B] = ???

      override def raiseError[A](e: String): Box[A] = ???

      override def handleErrorWith[A](fa: Box[A])(f: String => Box[A]): Box[A] = ???

      override def pure[A](x: A): Box[A] = Box(x)
    }
  }

  def first[F[_]: Applicative, A](a: A): F[A] = {
    Applicative[F].pure(a)
  }

  def second[F[_]: Applicative, A](a: A)(implicit monadError: MonadError[F, String]): F[A] = {
    monadError.pure(a)
  }

  def third[F[_]: Applicative, A](a: A)(implicit monadError: MonadError[F, String]): F[A] = {
    monadError.raiseError("third error")
  }

  def main(args: Array[String]): Unit = {
    val box = for {
      i <- Box(42)
      j <- Box(41)
      k <- first[Box, Int](1)
      l <- second[Box, Int](1)
      m <- third[Box, Int](1)
    }
    yield {
      i + j + k
    }
    println(box)
  }

}
