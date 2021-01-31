package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter11.Functor

object Chapter12 {

  trait Applicative[F[_]] extends Functor[F] {

    def unit[A](a: A): F[A]
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

    def traverse[A, B](as: List[A], f: A => F[B]): F[List[B]] = {
      as.foldRight[F[List[B]]](unit(List[B]()))(  (a: A, b: F[List[B]]) => map2(f(a), b) (_ :: _))
    }

    def sequenceUsingTraverse[A](fas: List[F[A]]): F[List[A]] = {
      traverse[F[A], A](fas, identity)
    }

    def sequence[A](fas: List[F[A]]): F[List[A]] = {
      fas.foldRight[F[List[A]]](unit[List[A]](List[A]()))((a: F[A], b: F[List[A]]) => map2(a, b)(_ :: _))
    }

    def replicateMUsingSequence[A](n: Int, fa: F[A]): F[List[A]] = {
      sequence(List.fill(n)(fa))
    }

    def replicateMUsingMap[A](n: Int, fa: F[A]): F[List[A]] = {
      map(fa)(a => List.fill(n)(a)) // It this what they call pure?
    }

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      map2(fa, fb)((_, _))
    }

  }

  object Ex1 {

    // Implemented sequence, replicateM and product


    def test: Unit = {

      // implemented

    }

  }

  object Ex2 {

  }

  def main(args: Array[String]): Unit = {
    Ex1.test
  }
}
