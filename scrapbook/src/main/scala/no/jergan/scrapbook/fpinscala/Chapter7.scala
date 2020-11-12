package no.jergan.scrapbook.fpinscala

import scala.annotation.tailrec
import scala.concurrent.duration.TimeUnit

/**
 * Second chapter done according to new version of book.
 */
object Chapter7 {

  case class Par[A](f: () => A)

  object Par {

    def unit[A](a: A): Par[A] = ???

    def lazyUnit[A](a: => A): Par[A] = {
      fork(unit(a))
    }

    def run[A](pa: Par[A]): A = ???

    def map2[A](pl: Par[A], pr: Par[A])(f: (A, A) => A): Par[A] = ???

    def fork[A](pa: => Par[A]): Par[A] = ???

  }

  def sum(ints: List[Int]): Par[Int] = {
    if (ints.size <= 1)
      Par.unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }

  class ExecutorService {
    def submit[A](a: Callable[A]): Future[A] = ???
  }

  trait Callable[A] {
    def call: A }

  trait Future[A] {
    def get: A
    def get(timeout: Long, unit: TimeUnit): A
    def cancel(evenIfRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean
  }

  object Ex1 {

    def test(): Unit = {

      type Par[A] = ExecutorService => Future[A]

      def run[A](s: ExecutorService)(a: Par[A]): Future[A] = {
        a(s)
      }



  }

  def main(args: Array[String]): Unit = {
    Ex1.test()
  }

}
