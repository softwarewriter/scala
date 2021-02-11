package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter11.Monad


object Chapter13 {

  sealed trait Free[F[_], A] {

    def map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
  }

  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  def freeMonad[F[_]]: Monad[({type f[x] = Free[F, x]})#f] = new Monad[({type f[x] = Free[F, x]})#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = FlatMap(fa, f)
  }

  @annotation.tailrec
  def runTrampoline[A](freeA: Free[Function0, A]): A = {
    freeA match {
      case Return(a) => a
      case Suspend(s) => s.apply()
      case FlatMap(s, f) => s match {
        case Return(a) => runTrampoline(f(a))
        case Suspend(s) => runTrampoline(f(s.apply()))
        case FlatMap(ss, g) => runTrampoline(ss.flatMap(a => g(a)).flatMap(f))
      }
    }
  }

  object Ex1 {
    // implemented map and flatMap in trait Free
    // implemented def freeMonad
  }

  object Ex2 {
    // implemented runTrampoline. IntelliJ struggles with the types but solution is correct and compiles with sbt.
  }

  def main(args: Array[String]): Unit = {

  }


  /*
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0,A]): A = (a) match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x,f) => x match {
      case Return(a) => runTrampoline { f(a) }
      case Suspend(r) => runTrampoline { f(r()) }
      case FlatMap(a0,g) => runTrampoline { a0 flatMap { a0 => g(a0) flatMap f } }
    }
  }

   */


}
