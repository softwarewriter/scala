package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter5.Stream
import no.jergan.scrapbook.fpinscala.Chapter6.State
import no.jergan.scrapbook.fpinscala.Chapter7.Par
import no.jergan.scrapbook.fpinscala.Chapter9.Parsers

object Chapter11 {

  trait JFunctor[A] {
    def map[B](f: A => B):  JFunctor[B]

    def andThen[B, C](f: A => B)(g: B => C): JFunctor[C] = {
      map(f).map(g)
    }
  }

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def andThen[A, B, C](fa: F[A])(f: A => B)(g: B => C): F[C] = {
      map(map(fa)(f))(g)
    }

    def mapPair[A, B, C](fab: F[(A, B)])(f: (A, B) => C): F[C] = {
      map(fab)(ab => f(ab._1, ab._2))
    }

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = {
      (map(fab)(_._1), map(fab)(_._2))
    }

    def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = {
      e match {
        case Left(fa) => map(fa)(Left(_))
        case Right(fb) => map(fb)(Right(_))
      }

      /*
         requires flatMap
      def join[A, B](fa: F[A], fb: F[B]): F[(A, B)]
       */

      /*
         requires flatMap
      def map2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] = {
        map(fa)(a => map(fb)(b => f(a, b)))
      }
       */
    }

    trait Monad[F[_]] extends Functor[F] {

      def unit[A](a: A): F[A]

      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
      //        map2[A, A, B](fa, fa)((a1, a2) => f(a1))

      def map[A, B](fa: F[A])(f: A => B): F[B] = {
        flatMap(fa)(a => unit(f(a)))
      }

      def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
        flatMap(fa)(a => map(fb)(b => f(a, b)))
      }

      def sequence[A](lfa: List[F[A]]): F[List[A]] = {
        lfa match {
          case Nil => unit(Nil)
          case l => flatMap(l.head)(h => map(sequence(l.tail))(t => h :: t))
        }
      }

      /*
      def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = {

      }

       */

    }

    object Monad {

      val parMonad: Monad[Par] = new Monad[Par] {
        override def unit[A](a: A): Par[A] = Par.unit(a)
        override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(fa)(f)
      }

      def parserMonad[Parser[+_]](p: Parsers[Parser]): Monad[Parser] = new Monad[Parser] {
        override def unit[A](a: A): Parser[A] = p.succeed(a)
        override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = p.flatMap(fa)(f)
      }

      val optionMonad: Monad[Option] = new Monad[Option] {
        override def unit[A](a: A): Option[A] = Some(a)
        override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
      }

      /*
         Should compile when we do chapter 5 according to new version.
      val streamMonad = new Monad[Stream] {
        override def unit[A](a: A): Stream[A] = Stream.apply(a)
        override def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] = fa.flatMap(f)
      }

       */

      val listMonad: Monad[List] = new Monad[List] {
        override def unit[A](a: A): List[A] = List(a)
        override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
      }


  }

  object Ex1 {
  }

  object Ex2 {
//    case class State[S, +A](run: S => (A, S))
    case class St()
    case class State2[+A](run: St => (A, St))

    def stateMonad[S]: Monad[State2] = new Monad[State2] {
      override def unit[A](a: A): State2[A] = ???
      override def flatMap[A, B](fa: State2[A])(f: A => State2[B]): State2[B] = ???
    }

    trait Monad2[F[_, _]] {
      def unit[A, B](a: A, b: B): F[A, B]
      def flatMap[A, B, C, D](fa: F[A, B])(f: (A, B) => F[C, D]): F[C, D]
    }

    val stateMonad2: Monad2[State] = new Monad2[State] {
      override def unit[A, B](a: A, b: B): State[A, B] = State.unit[A, B](b)
      override def flatMap[A, B, C, D](fa: State[A, B])(f: (A, B) => State[C, D]): State[C, D] = ???
    }

    }

  }

  object Ex3 {
  }

  def main(args: Array[String]): Unit = {

  }

}
