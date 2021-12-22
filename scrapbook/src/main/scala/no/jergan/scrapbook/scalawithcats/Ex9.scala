package no.jergan.scrapbook.scalawithcats

import cats.{Applicative, Monoid}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Ex9 {

  def foldMap[A, B: Monoid](l: List[A])(f: A => B): Future[B] = {
    println(s"${Thread.currentThread().getName}: $l")
    if (l.isEmpty) return Future(Monoid.empty)
    if (l.size == 1) return Future(f(l.iterator.next()))
    Future {
      val (a1, a2) = l.splitAt(l.length / 2)
      val f1 = foldMap(a1)(f)
      val f2 = foldMap(a2)(f)
      Applicative[Future].map2(f1, f2)((b1, b2) => Monoid[B].combine(b1, b2))
    }.flatten
  }

  def main(args: Array[String]): Unit = {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8)
    val f = foldMap(l)(_ * 2)
    Thread.sleep(1000)
    println(f)
  }

}
