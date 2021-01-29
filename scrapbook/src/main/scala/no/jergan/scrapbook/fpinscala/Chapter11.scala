package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter10.Monoid
import no.jergan.scrapbook.fpinscala.Chapter11.Monad.listMonad
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

    def join[A](ffa: F[F[A]]): F[A] = {
//      flatMap(ffa)(identity)
      flatMap[F[A], A](ffa)((fa: F[A]) => fa)
    }

    def flatMapUsingJoinAndMap[A, B](fa: F[A])(f: A => F[B]): F[B] = {
//      join(map(fa)(f))
      join(map[A, F[B]](fa)(a => f(a)))
    }

    def composeUsingJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a =>
      join(map(f(a))(g))

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
  }

  object Ex8 {
    // implemented flatMapUsingCompose
  }

  object Ex9 {
    // Show that associativity in composition of Kleisli arrows are the same as
    // associativity in flatMap

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

  object Pelle {
    // A => F[B]
    // observation: a Monioid over the type "endo" Kleisli arrow is associative and have a zero element.
    // this is the same as a flatMap of a Monad being associative.
    // and the unit of the Monad being the zero element.

    def kleisliArrowMonoid[F[_], A](monad: Monad[F]): Monoid[A => F[A]] = new Monoid[A => F[A]] {
      override def op(f: A => F[A], g: A => F[A]): A => F[A] = monad.compose(f, g)
      override val zero: A => F[A] = a => monad.unit(a)
    }

  }

  object Ex10 {
    /*
    use compose(f, g) == a => flatMap(f(a))(g)

    Left identity
    Prove that
    compose(unit, f) == f
    is equivalent to
    flatMap(unit(y))(f) == f(y)

    - compose(unit, f) == f
    - a => flatMap(unit(a))(f) == a => f
    - flatMap(unit(a))(f) == f(a)

    Right identity
    Prove that
    compose(f, unit) == f
    is equivalent to
    flatMap(x)(unit) == x

    - compose(f, unit) == f
    - a => flatMap(f(a)(unit) == a => f
    - flatMap(f(a)(unit) == f(a)
    - flatMap(x)(unit) == x

     */
  }

  object Ex11 {
    val optionMonad: Monad[Option] = new Monad[Option] {
      override def unit[A](a: A): Option[A] = Some(a)
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
        case None => None
        case Some(a) => f(a)
      }
    }

    /*
     Chose identity laws for the monad Option:
     Left identity
       flatMap(unit(y))(f) == f(y)
         f = _ => None
           flatMap(unit(y))(None) == None
         None == None
         f = _ => Some(a)
           flatMap(unit(y))(_ => Some(a)) == Some(y)
           flatMap(Some(y))(_ => Some(a)) == Some(y)
           Some(y) == Some(y)

     Right identity
       flatMap(x)(unit) == x
         x = None
           None == None
         x = Some(a)
           flatMap(Some(a))(unit) == Some(a)
           unit(a) == Some(a)
           Some(a) == Some(a)
     */

  }

  object Ex12 {
    // implemented join

    {
      val lls: List[List[String]] = ???
      val joined: List[String] = listMonad.join(lls)
    }

    {
      val llls: List[List[List[String]]] = ???
      val joined: List[List[String]] = listMonad.join(llls)
    }
  }

  object Ex13 {
    // implemented flatMapUsingJoinAndMap
    // implemented composeUsingJoinAndMap
  }

  object Ex14 {

    val fa: List[Int] = listMonad.unit(42)
    def f: Int => List[Int] = ???
    def g: Int => List[Int] = ???

    val r: List[Int] = fa
      .flatMap(a =>
        f(a)
          .flatMap(b =>
            g(b)
              .map(r => r)
          )
      )

    // Monad laws with join, map and unit

    /*
      Associative law for flatMap:
        ma.flatMap(f).flatMap(g) == ma.flatMap(a => f(a).flatMap(g))
        flatMap(flatMap(ma)(f))(g) == flatMap(ma)(a => flatMap(f(a)(g))

        Substitute: flatMap(fa)(f) with join(map(fa)(f)) to get
        join(map(join(map(ma)(f)))(g) == join(map(ma)(a => join(map(f(a)(g)))

      Right equality
        flatMap(ma)(unit) == ma
        Substitute: flatMap(fa)(f) with join(map(fa)(f)) to get
        join(map(fa)(unit)) == ma

      Left equality
        flatMap(unit(y))(f) == f(y)
        Substitute: flatMap(fa)(f) with join(map(fa)(f)) to get
        join(map(unit(y))(f) == f(y)

     */

  }

  object Ex15 {
    // Associative law for    Par: the order of which the Par are executed are should not effect the final result.
    // Associative law for Parser: if matching (a(bc)) it should also match ((ab)c)

  }

  object Ex16 {
    // Right identity law for List: for each element in a list, wrap in a list, then flatten the list => should give the original list
    //  Left identity law for list: wrap y in a list, unwrao, apply to f => should give f(y)

    // Right identity law for Gen: given a generator for a, take the a and wrap it into a new generator => should give a generator for the original value
    //  Left identity law for Gen: create a generator for y, unwrap the y and apply to f => should give a f(y)
  }

  object Ex17 {

    case class Id[A](value: A) {
      def map[B](f: A => B): Id[B] = Id(f(value))
      def flatMap[B](f: A => Id[B]): Id[B] = f(value)
    }

    val idMonad: Monad[Id] = new Monad[Id] {
      override def unit[A](a: A): Id[A] = Id(a)
      override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)
    }

    Id("hei").flatMap(a => Id())

  }

  def main(args: Array[String]): Unit = {

//    Thread.currentThread().wait()
    val v: String = Thread.currentThread().synchronized {
      Thread.currentThread().wait(10)
      "hei" }
    println(v)
//    Ex6.test()
  }

}
