package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter10.Monoid
import no.jergan.scrapbook.fpinscala.Chapter6.SimpleRNG
import no.jergan.scrapbook.fpinscala.Chapter8.Gen.{boolean, choose, int}
import no.jergan.scrapbook.fpinscala.Chapter8.Prop.{Falsified, Passed, Proved, Result}
import no.jergan.scrapbook.fpinscala.Chapter8.{Gen, Prop}

object Chapter10 {

  trait Monoid[A] {

    def op(a1: A, a2: A): A
    val zero: A

  }

  object Ex1 {

    val intAddition: Monoid[Int] = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 + a2
      override val zero: Int = 0
    }

    val intMultiplication: Monoid[Int] = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 * a2
      override val zero: Int = 1
    }

    val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
      override val zero: Boolean = false
    }

    val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
      override val zero: Boolean = true
    }
  }

  object Ex2 {

    def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
      override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
      override val zero: Option[A] = None
    }
  }

  object Ex3 {

    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
      override def op(a1: A => A, a2: A => A): A => A = a => {
        a1(a2(a))
      }
      override val zero: A => A = identity
    }
  }

  object Ex4 {

    def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
      val associative = Prop.forAll[List[A]](gen.listOfN(3)){l => {
        val a1 = l(0)
        val a2 = l(1)
        val a3 = l(2)
        m.op(a1, m.op(a2, a3)) == m.op(m.op(a1, a2), a3)
      }}.tag("Associative")
      val leftZero = Prop.forAll[A](gen){a => a == m.op(m.zero, a) }.tag("Left zero")
      val rightZero = Prop.forAll[A](gen){a => a == m.op(a, m.zero) }.tag("Right zero")
      associative
        .&&(leftZero)
        .&&(rightZero)
    }

    def option[A](genA: Gen[A]): Gen[Option[A]] = {
      boolean
        .flatMap(b => genA
          .map(a => if (b) Some(a) else None))
    }

    def endoFunction(genInt: Gen[Int]): Gen[Int => Int] = {
      genInt
        .map(i => _ + i)
    }

    def test(): Unit = {
      Prop.run(monoidLaws(Ex1.intAddition, int))
      Prop.run(monoidLaws(Ex1.intMultiplication, int))
      Prop.run(monoidLaws(Ex1.booleanOr, boolean))
      Prop.run(monoidLaws(Ex1.booleanAnd, boolean))
      Prop.run(monoidLaws(Ex2.optionMonoid[Int], option[Int](int)))
/*
    // TODO: How to compare two methods
      val f: Int => Int = Ex3.endoMonoid[Int].op(identity, identity)
      println(f)
      println(f(3))
      Prop.run(monoidLaws(Ex3.endoMonoid[Int], endoFunction(int)))
 */
    }

  }

  object Ex5 {

    def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
//    as.map(f).foldLeft(m.zero)(m.op)
//    or
      as.map(f).foldRight(m.zero)(m.op)
    }

    def test(): Unit = {
      val concat: Monoid[String] = new Monoid[String] {
        override def op(a1: String, a2: String): String = a1 + a2
        override val zero: String = ""
      }

      println(concatenate(List("ole", "dole", "doff"), concat))
    }
  }

  object Ex6 {

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {

      def ff: A => B = a => f(z, a)
      val m: Monoid[B] = new Monoid[B] {
        override def op(a1: B, a2: B): B = ???
        override val zero: B = z
      }

      val b = Ex5.foldMap[A, B](as, m)(ff)
      b
    }

    def test(): Unit = {
      println(foldLeft[String, String](List("ole", "dole", "doff"), "")(_ + _))
    }

  }

  def main(args: Array[String]): Unit = {
    Ex6.test()
  }

}
