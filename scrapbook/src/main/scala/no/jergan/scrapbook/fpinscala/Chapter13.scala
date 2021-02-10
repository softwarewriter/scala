package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter11.Monad


object Chapter13 {


  sealed trait Free[F[_], A] {

    /*
    def map[B](fa: Free[F, A])(f: A => B): Free[F, B] = {
     fa match {
        case Return(a) => Return(f(a))
        case Suspend(s) => FlatMap[F, A, B](Suspend(s), a => Return(f(a)))
        case FlatMap(s, ff) => {
          val ss: Free[F, A] = s
          val fff: Nothing => Free[F, A] = ff
        }
          FlatMap(s, a => f(a))
        }
    }

     */

  }

  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  def freeMonad[F[_]]: Monad[({type f[x] = Free[F, x]})#f] = new Monad[({type f[x] = Free[F, x]})#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = FlatMap(fa, f)
  }

  def main(args: Array[String]): Unit = {


    def f: Int => Int = a => {
//      println("f")
      a
    }

    val g = List.fill(10000)(f).foldLeft(f)(_ compose _)

    println(g(42))

  }


}
