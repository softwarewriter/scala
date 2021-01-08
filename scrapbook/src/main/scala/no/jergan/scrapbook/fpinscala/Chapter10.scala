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

  def inverseMonoid[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)
    override val zero: A = m.zero
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
      as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
//    or
//    as.map(f).foldLeft(m.zero)(m.op)
//    or ditto right
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

    def curryTest[A, B, C]() {
      def f(a: A, b: B): C = ???
      def ab(a: A): B => C = (b: B) => f(a, b)
      def ac(b: B): A => C = (a: A) => f(a, b)
      def abc: A => B => C = (a: A) => (b: B) => f(a, b)
    }

    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
      val aToBtoB: A => B => B = (a: A) => (b: B) => f(b, a)
      val m: Monoid[B => B] = inverseMonoid(Ex3.endoMonoid[B])
      val bToB: B => B = Ex5.foldMap[A, B => B](as, m)(aToBtoB)
      bToB(z)
    }

    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
      val aToBtoB: A => B => B = (a: A) => (b: B) => f(a, b)
      val m: Monoid[B => B] = Ex3.endoMonoid[B]
      val bToB: B => B = Ex5.foldMap[A, B => B](as, m)(aToBtoB)
      bToB(z)
    }

    def test(): Unit = {
      def f(z: Int, a: String): Int = z + a.length
      def ff: String => Int => Int = (a: String) => (z: Int) => f(z, a)
      def fff = ff("ole")
      println(fff(4))

      println(foldLeft[String, Int](List("ole", "dole", "doff"))(0)(_ + _.length))
      println(foldLeft[String, String](List("ole", "dole", "doff"))("left")(_ + _))
      println(foldRight[String, String](List("ole", "dole", "doff"))("right")(_ + _))
    }

  }

  object Ex7 {

    def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
      v.length match {
        case 0 => m.zero
        case 1 => m.op(m.zero, f(v.last))
        case _ => {
          val (l, r) = v.splitAt(v.length / 2)
          m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
        }
      }
    }

  }

  def main(args: Array[String]): Unit = {
    Ex6.test()
  }

}
