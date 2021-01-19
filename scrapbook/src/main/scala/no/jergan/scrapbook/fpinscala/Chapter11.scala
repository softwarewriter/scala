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
      def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
       */

      /*
         requires flatMap
      def map2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] = {
        map(fa)(a => map(fb)(b => f(a, b)))
      }
       */
    }
  }

  trait Monad[F[_]] extends Functor[F] {

    def unit[A](a: A): F[A]

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def flatMapUsingCompose[A, B](fa: F[A])(f: A => F[B]): F[B] = {
      val v: Any => F[B] = compose[Any, A, B](_ => fa, f)
      v(())
    }

    def map[A, B](fa: F[A])(f: A => B): F[B] = {
      flatMap(fa)(a => unit(f(a)))
    }

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      flatMap(fa)(a => map(fb)(b => f(a, b)))
    }

    def sequence[A](lma: List[F[A]]): F[List[A]] = {
      lma match {
        case Nil => unit(Nil)
        case l => flatMap(l.head)(h => map(sequence(l.tail))(t => h :: t))
      }
    }

    def sequenceUsingFoldRight[A](lma: List[F[A]]): F[List[A]] = {
      lma.foldRight(unit(List[A]()))((a, b) => map2(a, b)(_ :: _))
    }

    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
      sequence(la.map(f))
    }

    def traverseUsingFoldRight[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
      la.foldRight(unit(List[B]()))((a, b) => map2(f(a), b)(_ :: _))
    }

    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
      sequence(List.fill(n)(ma))
    }

    def filter[A](ms: List[A])(f: A => Boolean): List[A] = ???

    def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] = {
      as.foldRight(unit(List[A]()))((a, b) => map2(f(a), b)((bool, tail) => if (bool) a :: tail else tail))
    }

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a =>
      flatMap(f(a))(g)
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
    // Implemented monad instances
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

  object Ex3 {
    // Implemented sequence and traverse
  }

  object Ex4 {
    // Implemented replicateM
  }

  object Ex5 {
    //    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {

    // List(m): replicateM creates n * m Lists of List(n) with all combinations
    // Option: replicateM creates an Option of List(n) of the Option

    def test(): Unit = {

      println(Monad.optionMonad.replicateM(4, Some("ole")))
      println(Monad.listMonad.replicateM(4, List("ole", "dole")))
    }
  }

  object Ex6 {
    // implemented filterM

    // Option: filterM. For each a the Option[Boolean] can be either Some or None.
    // - If at least one is None the result is None
    // - If not, the result is Some(filteredList)

    def test(): Unit = {
      val list = List("ole", "dole", "doff")
      println(Monad.optionMonad.filterM(list)(a => if (a == "none") None else Some(a.contains("ole"))))
      println(Monad.listMonad.filterM(list)(a => if (a == "ole") List(true, false) else List(true)))
    }
  }

  object Ex7 {
    // Implemented compose

    // Monad laws
    // map
    // unit
    // flatMap
//    flatmap(a => (unit(f(a)))) === map(f)
//    unit(a).flatMap(f) = unit(f(a))

    /*
    val ps: Gen[(Double, Double, Double)] = for {
      price1 <- Gen.double()
      price2 <- Gen.double()
      price3 <- Gen.double()
    } yield (price1, price2, price3)

    val o: Option[String] = ???
    def f: String => Option[String] = ???
    def g: String => Option[String] = ???

    val r1: Option[String] = o
      .flatMap(f)
      .flatMap(g)

    val r2: Option[String] = o
      .flatMap(s => f(s).flatMap(g))

    Some(v).flatMap(f).flatMap(g) == Some(v).flatMap(a => f(a).flatMap(g))
    f(v).flatMap(g) == f(v).flatMap(g)

     */

//    compose(compose (f, g) h) === compose (f, compose(g, h))
  }

  object Ex8 {
    // implemented flatMapUsingCompose
  }

  object Ex9 {
    // Show that
    //   1) compose(compose(f, g), h) === compose(f, compose(g, h))
    // and
    //   2) flatMap(x)(f).flatMap(g) === flatMap(x)(a => flatMap(f(a))(g))
    // are equivalent

    // Start with 1) and work to 2)

    // Left: compose(compose (f, g), h)
    // substitute compose(f, g) by a => flatMap(f(a))(g)
    // ==> a => flatMap((b => flatMap(f(b))(g))(a))(h)
    // replace b by a as we invoke with a as argument
    // ==> a => flatMap(flatMap(f(a))(g))(h)
    // eliminate a and replace f(a) by x
    // ==> flatMap(flatMap(x)(g))(h)

    // Right: compose(f, compose(g, h)) ==> a1 => flatMap(f(a1))(a2 => flatMap(g(a2))(h))
    // substitute compose(f, g) by a => flatMap(f(a))(g)
    // ==> a => flatMap(f(a))(b => flatMap(g(b))(h))
    // eliminate a and replace f(a) by x
    // ==> flatMap(x)(b => flatMap(g(b))(h))
  }

  def main(args: Array[String]): Unit = {
    Ex6.test()
  }

}
