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
/*
  trait Callable[A] {
    def call: A
  }

 */

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown }
    latch.await
    ref.get
  }

  object Par {
    def unit[A](a: A): Par[A] = {
      es =>
        new Future[A] {
          def apply(cb: A => Unit): Unit = cb(a)
        }
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

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
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

          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

  }

  object TestingActors {

    def test(): Unit = {
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

      es.shutdown()
    }
  }

  def main(args: Array[String]): Unit = {
    TestingActors.test()
  }

}
