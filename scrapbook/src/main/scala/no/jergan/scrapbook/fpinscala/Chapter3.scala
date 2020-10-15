package no.jergan.scrapbook.fpinscala

import scala.annotation.tailrec

object Chapter3 {

  /*
  final case class Cons[A] (x: A, xs:List[A]) extends List[A] {
  }

  case object Nil extends List[Nothing] {

   */

  sealed trait Liste[+A]
  case object Nil extends Liste[Nothing]
  case class Cons[+A](head: A, tail: Liste[A]) extends Liste[A]

  def sum(ints: Liste[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: Liste[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): Liste[A] =
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: Liste[A]): Liste[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }
  }

  @tailrec
  def drop[A](l: Liste[A], n: Int): Liste[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (n == 0) l else drop(xs, n - 1)
    }
  }

  @tailrec
  def dropWhile[A](l: Liste[A], p: A => Boolean): Liste[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (p(x)) dropWhile(xs, p) else l
    }
  }

  object Ex1 {
    def test(): Unit = {
      val x: Int = apply(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
      }
      println(x)
    }
  }


  object Ex2 {

    def test() = {
      println(tail(apply(1, 2, 3)))
    }
  }

  object Ex3 {

    def test() = {
      println(drop(apply(1, 2, 3), 2))
    }
  }

  object Ex4 {

    def test() = {
      println(dropWhile(apply(1, 2, 3, 4), (a: Int) => a < 3))
    }
  }

  def main(args: Array[String]): Unit = {
    Ex4.test()
  }

}
