package no.jergan.scrapbook

import scala.annotation.tailrec

object TailRecursion {

  def handleTailRecursive[I, O](init: I, test: I => Option[O], next: I => I): O = {
    var currentI = init
    while (test(currentI).isEmpty) {
      currentI = next(currentI)
    }
    test(currentI).get
  }

  @tailrec
  def sum(soFar: Int, xs: List[Int]): Int = xs match {
    case Nil => soFar
    case head :: tail => sum(head + soFar, tail)
  }

  def sumIterative(xs: List[Int]): Int =
    handleTailRecursive[(Int, List[Int]), Int]((0, xs), { case (soFar, xs) => xs match {
      case Nil => Some(soFar)
      case _ => None
    }}, { case (soFar, xs) => (soFar + xs.head, xs.tail)})

  @tailrec
  def foldLeft[A, B](z: B)(xs: List[A])(f: (B, A) => B): B = xs match {
    case Nil => z
    case head :: tail => foldLeft(f(z, head))(tail)(f)
  }

  def foldLeftIterative[A, B](z: B, xs: List[A], f: (B, A) => B): B =
    handleTailRecursive[(B, List[A], (B, A) => B), B]((z, xs, f), {case (z, xs, _) => xs match {
      case Nil => Some(z)
      case _ => None
    }}, { case (z, xs, f) => (f(z, xs.head), xs.tail, f)})

  def foldRight[A, B](z: B)(xs: List[A])(f: (A, B) => B): B = xs match {
    case Nil => z
    case head :: tail => f(head, foldRight(z)(tail)(f))
  }

  def main(args: Array[String]): Unit = {
    println(foldLeft("")(List("A", "B", "C"))((b, a) => b ++ a))
    println(foldRight("")(List("A", "B", "C"))((a, b) => b ++ a))
    println(sumIterative(List(1, 2, 3, 4, 5)))
    println(foldLeftIterative[String, String]("", List("A", "B", "C"), (b, a) => b ++ a))
  }

}
