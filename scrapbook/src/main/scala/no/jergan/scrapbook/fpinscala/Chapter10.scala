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
      override val zero: A => A = a => a
    }
  }

  object Ex4 {

    def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {

      Prop.forAll[A](gen){a => println(a); true }
      /*
      Prop { (max, n, rng) => {

        def assosiative(a1: A, a2: A, a3: A): Boolean = {
          m.op(a1, m.op(a2, a3)) == m.op(m.op(a1, a2), a3)
        }

        def zeroLeft(a: A): Boolean = {
          m.op(m.zero, a) == a
        }

        def zeroRight(a: A): Boolean = {
          m.op(a, m.zero) == a
        }

        val list: List[A] = gen.listOfN(3).sample.run(rng)._1
        val a: A = list(0)
        println(a)
        if (zeroLeft(a) && zeroRight(a)) Passed else Falsified("Not zero", 1)
//        Falsified("Not zero", 1)
      }
      }

       */
    }

    def test(): Unit = {
      val monoid = Ex1.intAddition
      val gi: Gen[Int] = int()
      val monoidProp = monoidLaws(monoid, gi)
      Prop.run(monoidProp)
    }

  }

  def main(args: Array[String]): Unit = {
    Ex4.test()
  }

}
