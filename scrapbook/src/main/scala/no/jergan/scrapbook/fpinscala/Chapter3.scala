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

  def setHead[A](l: Liste[A], head: A): Liste[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(head, xs)
    }
  }

  def init[A](l: Liste[A]): Liste[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  def foldRight[A,B](l: Liste[A], z: B)(f: (A, B) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def reverse[A](l: Liste[A]): Liste[A] = {
    l match {
      case Nil => l
      case Cons(x, xs) => foldLeft(xs, Cons(x, Nil))((a: Liste[A], b: A) => Cons(b, a))
    }
  }

  @tailrec
  def foldLeft[A,B](l: Liste[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def foldLeftUsingRight[A,B](l: Liste[A], z: B)(f: (B, A) => B): B = ???

  def append[A](l1: Liste[A], l2: Liste[A]): Liste[A] = {
    (l1, l2) match {
      case (Nil, Nil) => Nil
      case (l1, Nil) => l1
      case (Nil, l2) => l2
      case (l1, l2) => foldRight(l1, l2)((a, b) => Cons(a, b))
    }
  }

  def flatten[A](l: Liste[Liste[A]]): Liste[A] = {
    (l) match {
      case Nil => Nil
      case _ => foldRight(l, Nil: Liste[A])((a, b) => append(a, b))
    }
  }

  def sumRight(l: Liste[Int]) = {
    foldRight(l, 0.0)(_ + _)
  }

  def productRight(l: Liste[Double]) = {
    foldRight(l, 1.0)((a, b) => a * b)
  }

  def lengthRight[A](l: Liste[A]) = {
    foldRight(l, 0)((_, b) => b + 1)
  }

  def sumLeft(l: Liste[Int]) = {
    foldLeft(l, 0.0)(_ + _)
  }

  def productLeft(l: Liste[Double]) = {
    foldLeft(l, 1.0)((a, b) => a * b)
  }

  def lengthLeft[A](l: Liste[A]) = {
    foldLeft(l, 0)((a, _) => a + 1)
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

  object Ex5 {
    def test() = {
      println(setHead(apply(1, 2, 3, 4), 42))
    }
  }

  object Ex6 {
    def test() = {
      println(init(apply(1, 2, 3, 4)))
    }
  }

  object Ex7 {
    def test() = {
      println(productRight(apply(1, 2, 0, 3, 4)))
      // Can not short-circuit if using foldRight
    }
  }

  object Ex8 {
    def test() = {
      println(foldRight(apply(1, 2, 3), Nil: Liste[Int])((a, b) => Cons(a, b)))
    }
  }

  object Ex9 {
    def test() = {
      println(lengthRight(apply(0, 0, 0)))
    }
  }

  object Ex10 {
    // Implemented foldLeft
  }

  object Ex11 {
    def test() = {
      println(sumLeft(apply(1, 2, 3)))
      println(productLeft(apply(1, 2, 4, 0)))
      println(lengthLeft(apply(1, 2, 3)))
    }
  }

  object Ex12 {
    def test() = {
      println(reverse(apply(1, 2, 3)))
    }
  }

  object Ex13 {
    def test() = {
      println(foldLeft(apply("a", "b", "c"), "z")((b, a) => b + ", " + a))
      println(foldLeftUsingRight(apply("a", "b", "c"), "z")((b, a) => b + ", " + a))

      // The other way is not possible, as foldLeft is tail recursive and foldRight is not.
      // If it was possible, it would be a curious performance improvement.
    }
  }

  object Ex14 {
    def test() = {
      println(append(apply(1, 2, 3), apply(4, 5, 6)))
    }
  }

  object Ex15 {
    def test() = {
      println(flatten(apply(apply(1, 2, 3), apply(4, 5, 6), apply(7, 8, 9))))
    }
  }

  def main(args: Array[String]): Unit = {
    Ex15.test()
  }

}
