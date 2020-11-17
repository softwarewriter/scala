package no.jergan.scrapbook.fpinscala

import java.util.concurrent.{Executors, TimeUnit}

import scala.::
import scala.annotation.tailrec
import scala.concurrent.duration.TimeUnit
import no.jergan.scrapbook.fpinscala.Chapter7.Par._

/**
 * Second chapter done according to new version of book.
 *
 * Exercise 10 is done in Chapter7NonBlocking.
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

  object Par {

    type Par[A] = ExecutorService => Future[A]

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

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

    def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
      map2(pa, unit(()))((a,_) => f(a))
    }

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] = {
      map(parList)(_.sorted)
    }

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
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


    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
      chooser(n)(i => choices(i))
    }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
      chooser(cond)(if (_) t else f)
    }

    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = {
      chooser[K, V](key)(key => choices.getOrElse(key, null))
    }

    // Denne signaturen (og implementasjonen) fant jeg selv!
    def chooser[S, A](selection: Par[S])(choices: S => Par[A]): Par[A] = {
      es =>
        choices(run(es)(selection).get)(es)
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

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
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
  }

  object Ex6 {
    // Implemented parFilter
  }

  object TrainingP110 {
    import Chapter7.Par.{map2, fork, unit}

    def fold[A, B](as: List[A], z: B)(f: A => B)(combine: (B, B) => B): Par[B] = {
      if (as.isEmpty) {
        unit(z)
      }
      else if (as.size <= 1) {
        unit(f(as.head))
      } else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(fork(fold(l, z)(f)(combine)), fork(fold(r, z)(f)(combine)))(combine(_, _))
      }
    }

    val sum1: Par[Int] = sum(List(1, 2, 3, 4).toIndexedSeq)
    val sum2: Par[Int] = fold[Int, Int](List(1, 2, 3, 4), 0)(identity)(_ + _)
    val max: Par[Int] = fold(List(1, 2, 3, 4), Int.MinValue)(identity)(Math.max)
    val min: Par[Int] = fold(List(1, 2, 3, 4), Int.MaxValue)(identity)(Math.min)

    def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
      val v1 = fork(map2(a, b)((a, b) => (a, b)))
      map2(v1, c)((ab, c) => f(ab._1, ab._2, c))
    }

    def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = {
      val v1: Par[(A, B)] = fork(map2(a, b)((_, _)))
      val v2: Par[(C, D)] = fork(map2(c, d)((_, _)))
      map2(v1, v2)((ab, cd) => f(ab._1, ab._2, cd._1, cd._2))
    }

    def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] = {
      val v1: Par[(A, B)] = fork(Par.map2(a, b)((_, _)))
      val v2: Par[(C, D)] = fork(Par.map2(c, d)((_, _)))
      val v3: Par[(A, B, C, D)] = fork(Par.map2(v1, v2)((ab, cd) => (ab._1, ab._2, cd._1, cd._2)))
      map2(v3, e)((abcd, e) => f(abcd._1, abcd._2, abcd._3, abcd._4, e))
    }

    def countWords(paragraphs: List[String]): Par[Int] = {
      fold(paragraphs, 0)(_.split(" ").length)(_ + _)
    }
  }

  object Ex7 {

    /*
 Vis at
   map(map(y)(g))(f) == map(y)(f compose g)

   Definisjoner:
   - map(unit(x))(f) == unit(f(x)) ()
   - y = unit(x) (definition)

      venstre
    map(map(y)(g))(f) ->
    map(map(unit(x))(g))(f) ->
    map(unit(g(x))(f) ->
    unit(f(g(x)) ->
    unit((f compose g)(x))

      høyre
    map(y)(f compose g) ->
    map(unit(x))(f compose g) ->
    unit((f compose g)(x))

     */

  }

  object Ex8 {
    // newFixedThreadPool(int nThreads)
  }

  object Ex9 {
    val n = 100
    def forker[A](as: List[A]): Par[A] = {
      if (as.length == 1) {
        Par.unit(as.head)
      }
      else {
        Par.fork(forker(as.tail))
      }
    }
    forker(List.fill(42)(n + 2))
  }

  object Ex11 {


    val v: Par[Int] = Par.unit(4)



  }

  def main(args: Array[String]): Unit = {
  }

}
