package no.jergan.scrapbook

import cats.Monad
import cats.data.EitherT
import cats.implicits._

/**
 * Java like dynamic proxy.
 */
object Proxy {

  trait TraitT[T[_, _]] {

    def f1[A, B]: Unit = {

    }
  }

  def f2: Unit = {


  }
  def map2[F[_], A, B, C](fa: F[A], fb: F[B], f: (A, B) => C): F[C] =
//    apply(fa, map[F, B, A => C](fb, b => a => f(a, b)))
  // apply(fb, map(fa, f.curried))
  apply(fb, map[F, A, B => C](fa, a => (b: B) => f(a, b)))

  def apply[F[_], A, B](fa: F[A], ff: F[A => B]): F[B] =
    map2[F, A, A => B, B](fa, ff, (a, f) => f(a))

  def unit[F[_], A](a: A): F[A] = ???

  def map[F[_], A, B](fa: F[A], f: A => B): F[B] =
    apply[F, A, B](fa, unit[F, A => B](f))


}
