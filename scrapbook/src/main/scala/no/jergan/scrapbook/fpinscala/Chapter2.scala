package no.jergan.scrapbook.fpinscala

import scala.annotation.tailrec

object Chapter2 {

  object Ex2 {

    @tailrec
    def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
      if (as.length < 2)
        true
      else
        gt(as.tail.head, as.head) && isSorted(as.tail, gt)
    }

    def test(): Unit = {
      val f: (Int, Int) => Boolean = (a, b) => a > b
      println(Ex2.isSorted(List().toArray, f))
      println(Ex2.isSorted(List(1).toArray, f))
      println(Ex2.isSorted(List(1, 2, 3).toArray, f))
      println(!Ex2.isSorted(List(1, 3, 2).toArray, f))
    }
  }

  object Ex3 {
    def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
      (b: B) => f(a, b)
    }

    def test(): Unit = {
      val f: (Int => String) = partial1[String, Int, String]("ole", (a: String, b: Int) => String.valueOf(a.length + b))
      println(f(3))
    }
  }

  object Ex4 {
    def curry[A, B, C](f: (A, B) => C): A => B => C = {
      (a: A) => (b: B) => f(a, b)
    }

    def test(): Unit = {
      def f: (String, String) => String = (a, b) => a + b
      println(curry(f)("ole")("dole"))
    }
  }

  object Ex5 {
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
      (a: A, b: B) => f(a)(b)
    }

    def test(): Unit = {
      def f: String => String => String = (a) => (b) => a + b
      println(uncurry(f)("ole", "dole"))
    }
  }

  object Ex6 {
    def compose[A,B,C](f: B => C, g: A => B): A => C = {
      (a: A) => f(g(a))
    }

    def test(): Unit = {
      def f: String => String = (a) => "f of " + a
      def g: String => String = (a) => "g of " + a
      println(compose(f, g)("ole"))
    }
  }

  def main(args: Array[String]): Unit = {
    Ex6.test()
  }

}
