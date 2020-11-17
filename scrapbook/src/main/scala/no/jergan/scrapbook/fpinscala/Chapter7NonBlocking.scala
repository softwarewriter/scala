package no.jergan.scrapbook.fpinscala

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}

/**
 * Second chapter done according to new version of book.
 *
 * Exercise 10 starts from here.
 */
object Chapter7NonBlocking {

  sealed trait Future[A] {
    private[fpinscala] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[Either[Exception, A]]

  def run[A](es: ExecutorService)(p: Par[A]): Either[Exception, A] = {
    val ref = new AtomicReference[Either[Exception, A]]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown }
    latch.await
    ref.get
  }

  object Par {
    def unit[A](a: A): Par[A] = {
      _ =>
        new Future[Either[Exception, A]] {
          def apply(callback: Either[Exception, A] => Unit): Unit = callback(Right(a))
        }
    }

    def lazyUnit[A](a: => A): Par[A] = {
      fork(unit(a))
    }

    def fork[A](a: => Par[A]): Par[A] = {
      es =>
        new Future[Either[Exception, A]] {
          def apply(cb: Either[Exception, A] => Unit): Unit = {
            try {
              val v: Future[Either[Exception, A]] = a(es)
              eval(es)(v(cb))
              //            eval(es)(a(es)(cb))
            }
            catch {
              case e: Exception => {println("fant deg"); println(e); cb(Left(e))}
            }
          }
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
      es => new Future[Either[Exception, C]] {
        def apply(cb: Either[Exception, C] => Unit): Unit = {
          var ar: Option[Either[Exception, A]] = None
          var br: Option[Either[Exception, B]] = None

          val combiner = Actor[Either[Either[Exception, A], Either[Exception, B]]](es) {
            case Left(a) => br match {
              case None => ar = Some(a)
              case Some(b) => eval(es)(cb(combine(a, b, f)))
            }

            case Right(b) => ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(cb(combine(a, b, f)))
            }
          }

          pa(es)(a => combiner ! Left(a))
          pb(es)(b => combiner ! Right(b))
        }
      }

    def combine[A, B, C](ea: Either[Exception, A], eb: Either[Exception, B], f: (A, B) => C): Either[Exception, C] = {
      (ea, eb) match {
        case (Right(a), Right(b)) => {
          try {
            Right(f(a, b))
          }
          catch {
            case e: Exception => {println(e); Left(e)}
          }
        }
        case _ => {println("pelle"); Left(new Exception("Pelle was here, at least one computation failed."))}
      }
    }

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
      fork(sequence(ps.map(asyncF(f))))
    }
  }

  def testLazyException() = {

    def f(ex: Boolean): String = {
      if (ex) {
        throw new NullPointerException("pelle")
      }
      "ole" + "dole"
    }

    def executor(s: => String): Unit = {
      println("ex1")
      println(s)
      println("ex2")
    }

    println("before execute")
    executor(f(true))

    // Exception is thrown when s is evaluated (at println(s) )

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

//      val p = parMap(List.range(1, 100))(math.sqrt(_))
     val p = parMap(List.range(1, 100))(i => if (i == 10) throw new NullPointerException("pelle") else math.sqrt(i))
      val x = run(es)(p)
      x match {
        case Left(exception) => exception.printStackTrace()
        case Right(a) => println(a)
      }
      println(x)

      es.shutdown()
    }
  }

  def main(args: Array[String]): Unit = {
//    testLazyException()
    TestingActors.test()
  }

}
