package no.jergan.scrapbook

import cats.Monad
import cats.data.EitherT
import cats.implicits._

/**
 * Nested for comprehensions.
 */
object ForComprehensions {

  def m[A[_]: Monad, B[_]: Monad]: Unit = {

    def a1(): A[String] = ???
    def a2(): A[Int] = ???
    def b3(): B[String] = ???
    def b4(): B[Int] = ???

    def convert[C](bc: B[C]): A[C] = ???

    val v = for {
      a1v <- a1()
      a2v <- a2()
      a34v <- convert(for {
        b3v <- b3()
        b4v <- b4()
      } yield {
        (b3v, b4v)
      })
    } yield {
      (a1v, a2v, a34v)
    }
  }

}
