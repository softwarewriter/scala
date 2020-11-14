package no.jergan.scrapbook.fpinscala

import java.util.concurrent.{Executors, TimeUnit}

import scala.concurrent.duration.TimeUnit

/**
 * Second chapter done according to new version of book.
 */
object Chapter7NonBlocking {

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
      val v: Seq[Par[Any]] = ps.map(asyncF(f))

      fork(sequence(ps.map(asyncF(f))))
    }

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      ps.foldRight(unit(List.empty[A]))((a, b) => map2(a, b)(_ :: _))
    }

    def sequenceBalanced[A](ps: List[Par[A]]): Par[List[A]] = {
      fork {
        if (ps.isEmpty) {
          unit(List.empty)
        }
        else if (ps.length == 1) {
          map(ps.head)(a => List(a))
        }
        else {
          val (l, r) = ps.splitAt(ps.length / 2)
          map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
        }
      }
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      as.map(asyncF(a => Option.when(f(a))(a)))
        .foldRight(unit(List.empty[A]))((a, b) => map2(a, b)((a, b) => a match {
          case Some(a) => a :: b
          case None => b
        }))
    }

    def parFilter2[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      map(
        sequence(
          as.map(asyncF(a => if (f(a)) List(a) else List.empty))
        )
      )(_.flatten)
    }

    def fork[A](a: => Par[A]): Par[A] = {
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })
    }

    def delay[A](fa: => Par[A]): Par[A] = {
      es => fa(es)
    }

    def asyncF[A, B](f: A => B): A => Par[B] = {
      a => lazyUnit(f(a))
    }

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

  object TestingActors {

    def test(): Unit = {
      val es = Executors.newFixedThreadPool(4)
      val echoer = Actor[String](es)(msg => /*throw new NullPointerException("pelle")*/println(s"Got message: $msg"), throwable => {
        println(s"Got throwable: $throwable")
        throw (throwable)
      }
      )

      echoer ! "hei"
      echoer ! "hei2"

    }
  }

  def main(args: Array[String]): Unit = {
    TestingActors.test()
  }

}
