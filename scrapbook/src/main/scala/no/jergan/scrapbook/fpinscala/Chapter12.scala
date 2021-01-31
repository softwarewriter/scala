package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter11.Functor

object Chapter12 {

  trait Applicative[F[_]] extends Functor[F] {

    def unit[A](a: => A): F[A]
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

    def map[A, B](fa: F[A])(f: A => B): F[B] = {
      map2[A, Any, B](fa, unit(()))( (a, b) => f(a))
    }

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

  trait Applicative2[F[_]] extends Functor[F] {

    def unit[A](a: => A): F[A]
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

    def map[A, B](fa: F[A])(f: A => B): F[B] = {
      apply[A, B](unit(f))(fa)
    }

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
/*
      def g: F[A => C] = apply[B, A => C]( /*F[B => (A => C)*/ unit(b => a => f(a, b))   )(fb)
      val fc = apply[A, C](g)(fa)
      fc
 */
//      apply(apply[B, A => C](unit(b => a => f(a, b)))(fb))(fa)
      apply(apply[A, B => C](unit(a => b => f(a, b)))(fa))(fb)
    }

    def applyUsingUnitAndMap2[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
      map2(fa, fab)((a, b) => b(a))
    }

    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
      apply(apply(apply[A, B => C => D](unit(a => b => c => f(a, b, c)))(fa))(fb))(fc)
    }

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
      apply(apply(apply(apply[A, B => C => D => E](unit(f.curried))(fa))(fb))(fc))(fd)
    }

  }

  object Ex1 {

    // Implemented sequence, replicateM and product


  }

  object Ex2 {
    // Implemented Applicative2
  }

  def main(args: Array[String]): Unit = {
  }
}
