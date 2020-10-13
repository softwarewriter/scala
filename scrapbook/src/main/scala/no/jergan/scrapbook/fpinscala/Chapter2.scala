package no.jergan.scrapbook.fpinscala

import scala.annotation.tailrec

object Chapter2 {

  object Ex2 {

    @tailrec
    def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
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

    def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
      (b: B) => f(a, b)
    }

    def test(): Unit = {
      val f: (Int => String) = partial1[String, Int, String]("ole", (a: String, b: Int) => String.valueOf(a.length + b))

      println(f(3))
    }


  }

  def main(args: Array[String]): Unit = {

    Ex3.test()
  }

}
