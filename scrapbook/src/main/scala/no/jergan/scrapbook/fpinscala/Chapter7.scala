package no.jergan.scrapbook.fpinscala

import java.util.concurrent.TimeUnit

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

    def lazyUnit[A](a: => A): Par[A] = {
      fork(unit(a))
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
      es => {
        val af = a(es)
        val bf = b(es)
        CombineFuture(af, bf, f)
      }
    }

    def map[A,B](pa: Par[A])(f: A => B): Par[B] = {
      map2(pa, unit(()))((a,_) => f(a))
    }

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] = {
      map(parList)(_.sorted)
    }

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
      sequence(ps.map(asyncF(f)))
    }

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
//      val v = unit(List.empty[A])
//      ps.foldRight(Par[List.empty])

      ps.foldRight(unit(List.empty[A]))((a, b) => map2(a, b)((c, d) => c :: d))
    }

    case class CombineFuture[A, B, C](fa: Future[A], fb: Future[B], f: (A, B) => C) extends Future[C] {
      def isDone = true
      def get: C = calculate(Long.MaxValue)
      def get(timeout: Long, units: TimeUnit): C = calculate(timeout)

      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false

      def calculate(timeout: Long): C = {
        val startA = System.currentTimeMillis()
        val a: A = fa.get(timeout, TimeUnit.MILLISECONDS)
        val stopA = System.currentTimeMillis()
        val b: B = fb.get(timeout - (stopA - startA), TimeUnit.MILLISECONDS)
        f(a, b)
      }
    }

    def fork[A](a: => Par[A]): Par[A] = {
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })
    }

    def asyncF[A, B](f: A => B): A => Par[B] = {
      a => lazyUnit(f(a))
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

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

    def test(): Unit = {
      println("pelle")

    }
  }

  object Ex2 {

    type Par[A] = () => A

  }

  object Ex3 {
    // Implemented CombineFuture
  }

  object Ex4 {
    // Implemented asyncF
  }

  object Ex5 {
    // Implemented sequence
    val v: Par[Int] = sum(List(1, 2, 3, 4))
  }

  def main(args: Array[String]): Unit = {
    Ex1.test()
  }

}
