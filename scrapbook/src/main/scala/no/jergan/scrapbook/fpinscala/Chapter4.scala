package no.jergan.scrapbook.fpinscala

import scala.annotation.tailrec

object Chapter4 {

  sealed trait Option[+A] {

    // match allowed
    def map[B](f: A => B): Option[B] = {
      this match {
        case Some(value) => Some(f(value))
        case None => None
      }
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      map(f).getOrElse(None)
    }

    // match allowed
    def getOrElse[B >: A](default: => B): B = {
      this match {
        case Some(value) => value
        case None => default
      }
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      this match {
        case Some(value) => Some(value)
        case None => ob
      }
    }

    def filter(f: A => Boolean): Option[A] = {
      flatMap(value => if (f(value)) Some(value) else None)
    }

  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Ex1 {

    def test() = {
      class A1
      class A2 extends A1
      class A3 extends A2

      def elseProvider: A2 = {
        println("evaluate else1")
        new A2
      }

      Some(new A2).getOrElse(elseProvider)
      None.getOrElse(elseProvider)

      def elseProvider2: Option[A2] = {
        println("evaluate else2")
        Some(new A2)
      }
      Some(new A2).orElse(elseProvider2)
      None.orElse(elseProvider2)

      println(None.orElse(None).orElse(None).orElse(Some(new A2)))

    }

  }

  object Ex2 {

    def mean(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) None else Some(xs.sum / xs.size)
    }

    def variance(xs: Seq[Double]): Option[Double] = {
      mean(xs)
        .flatMap(m => if (xs.size < 2) None else Some(xs.map(x => math.pow(x - m, 2)).sum / (xs.size - 1)))
    }
  }

  object Ex3 {
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      a.flatMap(aa => b.map(bb => f(aa, bb)))
    }

    def test() = {
      println(map2(Some("a"), Some("b"))((a, b) => a + " and " + b))
    }
  }

  def main(args: Array[String]): Unit = {
    Ex3.test()
  }

}
