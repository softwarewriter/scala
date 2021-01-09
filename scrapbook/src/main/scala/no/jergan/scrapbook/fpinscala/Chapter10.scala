package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter10.Ex10.{WC, blank, wcMonoid}
import no.jergan.scrapbook.fpinscala.Chapter10.Ex12.Foldable
import no.jergan.scrapbook.fpinscala.Chapter10.Ex7.foldMapV
import no.jergan.scrapbook.fpinscala.Chapter8.Gen.{boolean, int}
import no.jergan.scrapbook.fpinscala.Chapter8.{Gen, Prop}

import scala.annotation.tailrec

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
        case 1 => f(v.last)
        case _ => {
          val (l, r) = v.splitAt(v.length / 2)
          m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
        }
      }
    }

  }

  object Ex8 {

    import no.jergan.scrapbook.fpinscala.Chapter7._

    def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
      override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
      override val zero: Par[A] = Par.unit(m.zero)
    }

    def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
      val parBs = Par.parMap(as.toList)(f)
//      Par.map(parBs)(bs => foldMapV[B, B](bs.toIndexedSeq, m)(identity))
      Par.flatMap(parBs)(bs => foldMapV[B, Par[B]](bs.toIndexedSeq, par(m))(b => Par.unit(b)))
    }

  }

  object Ex9 {

    def isOrdered(is: IndexedSeq[Int]): Boolean = {

      sealed trait Span

      case object Edge extends Span
      case object NonSorted extends Span
      case class MinMax(min: Int, max: Int) extends Span

      val m: Monoid[Span] = new Monoid[Span] {
        override def op(s1: Span, s2: Span): Span = (s1, s2) match {
          case (NonSorted, _) => NonSorted
          case (_, NonSorted) => NonSorted
          case (Edge, s) => s
          case (s, Edge) => s
          case (s1: MinMax, s2: MinMax) => if (s1.max > s2.min) NonSorted else MinMax(s1.min, s2.max)
        }
        override val zero: Span = Edge
      }
      val span = Ex5.foldMap(is.toList, m)((i: Int) => MinMax(i, i))
      span match {
        case NonSorted => false
        case _ => true
      }
    }

    def test(): Unit = {
      println(isOrdered(List().toIndexedSeq))
      println(isOrdered(List(1, 2, 3, 4).toIndexedSeq))
      println(isOrdered(List(1, 2, 4, 3).toIndexedSeq))
      println(isOrdered(List(2, 1, 3, 4).toIndexedSeq))
    }
  }

  object Ex10 {
    case class WC(leftBlank: Boolean, words: Int, rightBlank: Boolean)
    val blank = " "
    val wcMonoid: Monoid[WC] = new Monoid[WC] {
      override def op(a1: WC, a2: WC): WC =
        WC(a1.leftBlank, a1.words + a2.words + combine(a1.rightBlank, a2.leftBlank), a2.rightBlank)
      override val zero: WC = WC(true, 0, true)

      def combine(b1: Boolean, b2: Boolean): Int = {
        if (b1 && !b2) 1 else 0
      }
    }
  }

  object Ex11 {

    def count(s: String): Int = {
      foldMapV[Char, WC](blank + s + blank, wcMonoid)((c: Char) => WC(c.toString.equals(blank), 0, c.toString.equals(blank)))
        .words
    }
    def test(): Unit = {
      println(count(""))
      println(count("ole"))
      println(count(" ole "))
      println(count("ole eller dole"))
    }
  }

  object Ex12 {
    trait Foldable[F[_]] {
      def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
      def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
      def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
      def concatenate[A](as: F[A])(m: Monoid[A]): A =
        foldLeft(as)(m.zero)(m.op)
    }

    object FoldableList extends Foldable[List] {

      override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
        as match {
          case Nil => z
          case l => f(l.head, foldRight(l.tail)(z)(f))
        }
      }

      @tailrec
      override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
        as match {
          case Nil => z
          case l => foldLeft(l.tail)(f(z, l.head))(f)
        }
      }

      override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
        foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
    }

    object FoldableIndexedSeq extends Foldable[IndexedSeq] {
      @tailrec
      override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
        if (as.isEmpty) z else foldRight(as.splitAt(as.length - 1)._1)(f(as.last, z))(f)

      @tailrec
      override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
        if (as.isEmpty) z else foldLeft(as.splitAt(1)._2)(f(z, as.head))(f)

      override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = {
        foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
      }
    }

    object FoldableStream extends Foldable[Chapter5.Stream] {
      override def foldRight[A, B](as: Chapter5.Stream[A])(z: B)(f: (A, B) => B): B = {
        as.uncons match {
          case Some((h, t)) => f(h, foldRight(t)(z)(f))
          case None => z
        }
      }

      override def foldLeft[A, B](as: Chapter5.Stream[A])(z: B)(f: (B, A) => B): B = {
        as.uncons match {
          case Some((h, t)) => foldLeft(t)(f(z, h))(f)
          case None => z
        }
      }

      override def foldMap[A, B](as: Chapter5.Stream[A])(f: A => B)(mb: Monoid[B]): B =
        foldLeft(as)(mb.zero)((b: B, a: A) => mb.op(b, f(a)))
    }

  }

  object Ex13 {

    // Changed from "case object Leaf" to "case class Branch"

    sealed trait Tree[A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    object FoldableTree extends Foldable[Tree] {
      override def foldRight[A, B](t: Tree[A])(z: B)(f: (A, B) => B): B =
        t match {
          case Leaf(v) => f(v, z)
          case Branch(l, r) => foldRight(r)(foldRight(l)(z)(f))(f)
        }

      override def foldLeft[A, B](t: Tree[A])(z: B)(f: (B, A) => B): B =
        t match {
          case Leaf(v) => f(z, v)
          case Branch(l, r) => foldLeft(l)(foldLeft(r)(z)(f))(f)
        }
      override def foldMap[A, B](t: Tree[A])(f: A => B)(mb: Monoid[B]): B =
        t match {
          case Leaf(v) => f(v)
          case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
        }
    }

    object Ex14 {

      object FoldableOption extends Foldable[Option] {
        override def foldRight[A, B](o: Option[A])(z: B)(f: (A, B) => B): B = {
          o match {
            case Some(a) => f(a, z)
            case None => z
          }
        }

        override def foldLeft[A, B](o: Option[A])(z: B)(f: (B, A) => B): B =
          o match {
            case Some(a) => f(z, a)
            case None => z
          }

        override def foldMap[A, B](o: Option[A])(f: A => B)(mb: Monoid[B]): B =
          o match {
            case Some(a) => f(a)
            case None => mb.zero
          }
      }
    }

    /*
    object Ex15 {
      // Changed from "(fa: F[A])" to "(fa: Foldable[A])"

      def toList[A](fa: Foldable[A]): List[A] = {

        fa.foldRight(fa)(List[A]())()
      }
    }

     */


  }

  def main(args: Array[String]): Unit = {
    Ex11.test()
  }

}
