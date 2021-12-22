package no.jergan.scrapbook.scalawithcats

import cats.Bimonad.ops.toAllBimonadOps
import cats.effect.Sync
import cats.{Applicative, Foldable, Monoid}
import cats.implicits.{toFlatMapOps, toFunctorOps, toTraverseOps}
import no.jergan.scrapbook.fpinscala.Chapter12.Applicative

object Ex713 {

  def main(args: Array[String]): Unit = {

    def map[A, B](l: List[A])(f: A => B): List[B] = l.foldRight(List[B]())((a, b) => f(a) :: b)

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l.foldRight(List[B]())((a, b) => f(a) appendedAll b)

    def filter[A](l: List[A])(p: A => Boolean): List[A] = l.foldRight(List[A]())((a, b) => if (p(a)) a :: b else b)

    def combineAll[A: Monoid](l: List[A]): A = l.foldRight(Monoid[A].empty)((a, b) => Monoid[A].combine(a, b))

    def sum[A: Monoid](l: List[A]): A = combineAll(l)

    def foldMap[A, B: Monoid](l: List[A])(f: A => B): B =
      l.foldRight(Monoid[B].empty)((a, b) => Monoid[B].combine(f(a), b))

    val l = List(1, 2, 3)

    println(map(l)(_ + 1))
    println(flatMap(l)(a => List(a, a)))
    println(filter(l)(a => a > 1))
    println(sum(l))
    println(foldMap(List("1", "2", "3"))(s => Integer.parseInt(s)))
  }

}
