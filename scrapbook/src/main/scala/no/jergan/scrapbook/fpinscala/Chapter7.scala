package no.jergan.scrapbook.fpinscala

import scala.annotation.tailrec

/**
 * Second chapter done according to new version of book.
 */
object Chapter7 {

  trait Par[A]

  object Par {

    def unit[A](a: => A): Par[A] = ???

    def get[A](a: Par[A]): A = ???

    def map2[A](pl: Par[A], pr: Par[A])(f: (A, A) => A): Par[A] = ???

    //  Par.map2(sum(l), sum(r))(_ + _)


  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
    Par.unit(ints.headOption getOrElse 0) else {
    val (l, r) = ints.splitAt(ints.length / 2)
    Par.map2(sum(l), sum(r))(_ + _)
  }

  object Ex1 {

    def test(): Unit = {


    }
  }

  def main(args: Array[String]): Unit = {
    Ex1.test()
  }

}
