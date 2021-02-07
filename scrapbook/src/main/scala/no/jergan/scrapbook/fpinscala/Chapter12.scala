package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter10.Ex12.Foldable
import no.jergan.scrapbook.fpinscala.Chapter10.Monoid
import no.jergan.scrapbook.fpinscala.Chapter11.{Functor, Monad}
import no.jergan.scrapbook.fpinscala.Chapter6.State

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

    def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
      ofa.foldRight[F[Map[K, V]]](unit[Map[K, V]](Map[K, V]())){case ((k, fv), b) => map2(fv, b)((v: V, b2: Map[K, V]) => b2 + (k -> v))}
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

    def map2UsingMapAndProduct[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      map(product(fa, fb))(a => f(a._1, a._2))
    }

    def map3UsingMap2[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
      map2(map2(fa, fb)((_, _)), fc)((a, b) => f(a._1, a._2, b))
    }

    def product[G[_]](G: Applicative[G]) =  {
      val self = this

      new Applicative[({type f[x] = (F[x], G[x])})#f] {
        override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

        override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
          (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
      }
    }

    def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {

      val F: Applicative[F] = this

      new Applicative[({type f[x] = F[G[x]]})#f] {
        override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
        override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] = {
          F.map2[G[A], G[B], G[C]](fa, fb)(G.map2[A, B, C](_, _)(f))
        }
      }
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

    def map2UsingOneApplyOnly[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      apply(map(fa)(f.curried))(fb)
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

  trait Traverse[F[_]] extends Functor[F] {

    case class Id[A](a: A)

    implicit val applicativeId: Applicative[Id] = new Applicative[Id] {
      override def unit[A](a: => A): Id[A] = Id(a)

      override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = unit(f(fa.a, fb.a))
    }

    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

    def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
      traverse(fga)(ga => ga)

    def map[A, B](fa: F[A])(f: A => B): F[B] = {
      traverse[Id, A, B](fa)(a => applicativeId.unit(f(a))).a
    }

    //    def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] = traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

    def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = ???

    def toList[A](fa: F[A]): List[A] =
      mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

    def reverse[A](fa: F[A]): F[A] = {
      mapAccum[List[A], A, A](fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1
    }

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
      mapAccum[B, A, Any](as, z)((a, s) => ((), f(s, a)))._2
    }

    def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B]) (G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
      implicit val GH = G.product(H)
      traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))
    }

    def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = {
      val F = this
      new Traverse[({type f[x] = F[G[x]]})#f] {
        override def traverse[AP[_]: Applicative, A, B](fga: F[G[A]])(f: A => AP[B]): AP[F[G[B]]] = {
          F.traverse(fga)(G.traverse(_)(f))
        }
      }
    }
  }

  object Ex1 {

    // Implemented sequence, replicateM and product


  }

  object Ex2 {
    // Implemented Applicative2
  }

  object Ex3 {
    // Implemented map3 and map4
  }

  object E4 {
    // What is the meaning of
    // def sequence[A](a: List[Stream[A]]): Stream[List[A]]

    // List[Stream[A]] => Stream[List[A]] where each element in the new stream is a list of elements from elements of the input streams
    // (from corresponding position)

  }

  object Ex5 {

    def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {

      override def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
        case Right(a) => f(a)
        case Left(l) => Left[E, B](l)
      }
    }
  }

  object Ex6 {

    /*
    def eitherApplicable[E] = new Applicative[({type f[x] = Either[E, x]})#f] {
      override def unit[A](a: => A): Either[E, A] = Right(a)
      override def map2[A, B, C](fa: Either[E, A], fb: Either[E, B])(f: (A, B) => C): Either[E, C] = {
        (fa, fb) match {
          case (Right(a), Right(b)) => Right(f(a, b))
          case (Left(a), Right(_)) => Left(a)
          case (Right(_), Left(b)) => Left(b)
          case (Left(a), Left(b)) => Left( g(a, b))
        }
      }
    }

     */

    sealed trait Validation[+E, +A]

    case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
    case class Success[A](a: A) extends Validation[Nothing, A]

    def validationApplicable[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)
      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
        case (Success(a), Success(b)) => unit(f(a, b))
        case (Success(_), Failure(hb, tb)) => Failure(hb, tb)
        case (Failure(ha, ta), Success(_)) => Failure(ha, ta)
        case (Failure(ha, ta), Failure(hb, tb)) => Failure(ha, ta.appended(hb).appendedAll(tb))
      }
    }

  }

  object Ex6_2 {
    // implemented map2UsingMapAndProduct
  }

  object Ex7 {

    // Prove that if the monads laws hold, the monad-implementation of map and map2 satisfy the applicative laws.
    /*

      monad-Map:

    def map[A, B](fa: F[A])(f: A => B): F[B] = {
      flatMap(fa)(a => unit(f(a)))
    }

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      flatMap(fa)(a => map(fb)(b => f(a, b)))
    }

    Requirements for map (from functor):
    -
      map(v)(identity) == v
      map(map(v)(g))(f) == map(v)(f compose g)

      map(v)(identity) --> flatMap(v)(a => unit(identity(a))) --> v
      Left: map(map(v)(g))(f) --> flatMap(flatMap(v)(a => unit(g(a)))(b => unit(g(b)))
      Right: map(v)(f compose g) --> flatMap(v)(a => unit((f compose g)(a)))

     */


  }

  object Ex8 {
    // Implemented product (of Applicatives)
  }

  object Ex9 {
    // Implemented compose (of Applicatives)
  }

  object Ex10 {
    // Skipped as this is to be "extremely challenging"
  }

  object Ex11 {
    // Tried to implement compose of Monads. This stops when we try to implement flatMap
  }

  object Ex12 {
    // Implemented sequenceMap
  }

  object Ex13 {
    // Implemented Traverse for List, Option and Tree

    val listTraverse = new Traverse[List] {
      override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
        val AG = implicitly[Applicative[G]]
        fa.foldRight(AG.unit(List[B]()))((ga, gb) => AG.map2(f(ga), gb)(_ :: _))
      }
    }

    val optionTraverse = new Traverse[Option] {
      override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = {
        val AG = implicitly[Applicative[G]]
        fa match {
          case Some(a) => AG.map(f(a))(Some(_))
          case None => AG.unit(None)
        }
      }
    }

    case class Tree[+A](head: A, tail: List[Tree[A]])

    val treeTraverse = new Traverse[Tree] {

      override def traverse[G[_] : Applicative, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] = {
        val AG = implicitly[Applicative[G]]
        AG.map2(f(fa.head), listTraverse.traverse(fa.tail)(traverse(_)(f)))(Tree(_, _))
      }
    }
  }

  object Ex14 {

    // implemented map on traverse
  }

  object Ex15 {
    // Why can't any Foldable be a functor?

    trait Foldable[F[_]] extends Functor[F] {
      def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

      override def map[A, B](fa: F[A])(f: A => B): F[B] = {
        val mb: Monoid[F[B]] = ???
        val afb: A => F[B] = ???
        foldMap[A, F[B]](fa)(afb)(mb)
      }
    }

    // We can't implement mb and afb in general.

    trait FoldableList extends Functor[List] {
      def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B

      override def map[A, B](fa: List[A])(f: A => B): List[B] = {
        val mb: Monoid[List[B]] = new Monoid[List[B]] {
          override val zero: List[B] = List()
          override def op(a1: List[B], a2: List[B]): List[B] = a1.appendedAll(a2)
        }
        val afb: A => List[B] = a => List(f(a))
        foldMap[A, List[B]](fa)(afb)(mb)
      }
    }
  }

  object Ex16 {
    // Implemented reverse. Still don't understand what this means.
  }

  object Ex17 {
    // implemented foldLeft
  }

  object Ex18 {
    // implemented fuse
  }

  object Ex19 {
    // implemented compose
  }

  object Ex20 {

    def composeM[F[_], G[_]](F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f] = {

      new Monad[({type f[x] = F[G[x]]})#f] {
        override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
        override def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] = {

          def applicative[F[_]](monad: Monad[F]): Applicative[F] = new Applicative[F] {
            override def unit[A](a: => A): F[A] = monad.unit(a)
            override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = monad.map2(fa, fb)(f)
          }
          val AF = applicative(F)
//          val v = F.flatMap(fga)(ga => G.flatMap(ga)(a => G.unit(f(a))))
//          F.flatMap(fga)(ga => F.map(T.sequence(G.flatMap(ga)(a => G.unit(f(a))))(AF))(ggb => G.flatMap(ggb)(identity)))
//          F.flatMap(fga)(ga => F.map(T.traverse(ga)(f)(AF))(ggb => G.flatMap(ggb)(identity)))
          F.flatMap(fga)(ga => F.map(T.traverse(ga)(f)(AF))(G.join))
        }
      }
    }

    // implemented composeM
  }

  object ExE2 {

    case class OptionT[M[_], A](value: M[Option[A]])(implicit M: Monad[M]) {
      def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] = {
        OptionT(M.flatMap(value) {
          case Some(a) => f(a).value
          case None => M.unit(None)
        })
      }
    }
  }

  def main(args: Array[String]): Unit = {
  }

}
