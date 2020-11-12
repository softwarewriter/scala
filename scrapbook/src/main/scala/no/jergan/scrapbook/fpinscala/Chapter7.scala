package no.jergan.scrapbook.fpinscala

import scala.annotation.tailrec
import scala.concurrent.duration.TimeUnit

/**
 * Second chapter done according to new version of book.
 */
object Chapter7 {

  class ExecutorService {
    def submit[A](a: Callable[A]): Future[A] = ???
  }

  trait Callable[A] {
    def call: A
  }

  trait Future[A] {
    def get: A
    def get(timeout: Long, unit: TimeUnit): A
    def cancel(evenIfRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean
  }

  type Par[A] = ExecutorService => Future[A]

  object Par {

    def unit[A](a: A): Par[A] = {
      (_: ExecutorService) => UnitFuture(a)
    }

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }


    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
      es => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }
    }

    def map2My[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
      case class MyFuture(fa: Future[A], fb: Future[B]) extends Future[C] {
        def isDone = true
        def get: C = f(fa.get, fb.get)
        def get(timeout: Long, units: TimeUnit): C = {
          val startA = System.currentTimeMillis()
          val a: A = fa.get(timeout, units)
          val stopA = System.currentTimeMillis()
          val b: B = fb.get(timeout - (stopA - startA), units)
          f(a, b)
        }
        def isCancelled = false
        def cancel(evenIfRunning: Boolean): Boolean = false
      }
      es => {
        val af = a(es)
        val bf = b(es)
        MyFuture(af, bf)
      }
    }

    def fork[A](a: => Par[A]): Par[A] = {
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })
    }
  }

  def sum(ints: List[Int]): Par[Int] = {
    if (ints.size <= 1)
      Par.unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }

  object Ex1 {

    def test(): Unit = {
      println("pelle")

    }
  }

  def main(args: Array[String]): Unit = {
    Ex1.test()
  }

}
