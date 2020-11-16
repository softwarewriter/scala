package no.jergan.scrapbook.fpinscala

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}

/**
 * Second chapter done according to new version of book.
 *
 * Exercise 10 starts from here.
 */
object Chapter7NonBlocking {

  /*
  class ExecutorService {
    def submit[A](a: Callable[A]): Future[A] = ???
  }

 */

  sealed trait Future[A] {
    private[fpinscala] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown }
    latch.await
    ref.get
  }

  object Par {
    def unit[A](a: A): Par[A] = {
      _ =>
        new Future[A] {
          def apply(cb: A => Unit): Unit = cb(a)
        }
    }

    def lazyUnit[A](a: => A): Par[A] = {
      fork(unit(a))
    }

    def fork[A](a: => Par[A]): Par[A] = {
      es =>
        new Future[A] {
          def apply(cb: A => Unit): Unit =
            eval(es)(a(es)(cb))
        }
    }

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] {
        def call = r
      })

    def asyncF[A, B](f: A => B): A => Par[B] = {
      a => lazyUnit(f(a))
    }

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      fork {
        if (ps.isEmpty) {
          unit(List.empty)
        }
        else if (ps.length == 1) {
          map(ps.head)(a => List(a))
        }
        else {
          val (l, r) = ps.splitAt(ps.length / 2)
          map2(sequence(l), sequence(r))(_ ++ _)
        }
      }
    }

    def map[A,B](pa: Par[A])(f: A => B): Par[B] = {
      map2(pa, unit(()))((a,_) => f(a))
    }

    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None

          val combiner = Actor[Either[A, B]](es) {
            case Left(a) => br match {
              case None => ar = Some(a)
              case Some(b) => eval(es)(cb(f(a, b)))
            }

            case Right(b) => ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(cb(f(a, b)))
            }
          }

          pa(es)(a => combiner ! Left(a))
          pb(es)(b => combiner ! Right(b))
        }
      }

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
      fork(sequence(ps.map(asyncF(f))))
    }
  }

  object TestingActors {

    def test(): Unit = {
      import Par._

      println(Thread.currentThread().getName)
      val es = Executors.newFixedThreadPool(4)
      val echoer = Actor[String](es)(msg => {
        println("In actor: " + Thread.currentThread().getName)
        println(s"Got message: $msg")
      }, throwable => {
        println(s"Got throwable: $throwable")
        throw (throwable)
      }
      )
      echoer ! "hei"
      echoer ! "hei2"

//      val p = parMap(List.range(1, 1000))(math.sqrt(_))
      val p = parMap(List.range(1, 1000))(i => if (i == 100) throw new NullPointerException("pelle") else math.sqrt(i))
      val x = run(es)(p)
      println(x)

      es.shutdown()
    }
  }

  def main(args: Array[String]): Unit = {
    TestingActors.test()
  }

}
