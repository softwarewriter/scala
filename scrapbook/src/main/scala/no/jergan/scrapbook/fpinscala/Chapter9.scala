package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter8.Prop.forAll
import no.jergan.scrapbook.fpinscala.Chapter8.{Gen, Prop}

object Chapter9 {

  trait Parsers[ParseError, Parser[+_]] {

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    def char(c: Char): Parser[Char] = {
      string(c.toString).map(_.charAt(0))
    }

    def succeed[A](a: A): Parser[A] = {
      val v: Parser[String] = string("")
      string("").map(_ => a)
    }

    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

    def many[A](p: Parser[A]): Parser[List[A]]

    def many1[A](p: Parser[A]): Parser[List[A]] = {
      map2(p, many(p))((a, b) => a :: b)
    }

    def map[A, B](a: Parser[A])(f: A => B): Parser[B]

    def map2[A, B, C](a: Parser[A], b: Parser[B])(f: (A, B) => C): Parser[C] = {
      map(product(a, b))(t => f(t._1, t._2))
    }

    def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

    def slice[A](p: Parser[A]): Parser[String]

    def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)]

    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

//    implicit def a[A](a: A): Parser[A]
//    implicit def asAParser[A](a: A)(implicit f: A => Parser[A]): ParserOps[A] = ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = Parsers.this.or(p, p2)
      def or[B >: A](p2: => Parser[B]): Parser[B] = Parsers.this.or(p, p2)
      def many: Parser[List[A]] = Parsers.this.many(p)
      def map[B](f: A => B): Parser[B] = Parsers.this.map(p)(f)
      def flatMap[B](f: A => Parser[B]): Parser[B] = Parsers.this.flatMap(p)(f)
      def slice: Parser[String] = Parsers.this.slice(p)
      def **[B](p2: Parser[B]): Parser[(A, B)] = Parsers.this.product(p, p2)
      def product[B](p2: => Parser[B]): Parser[(A, B)] = Parsers.this.product(p, p2)

      object Laws {
        def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
          forAll(in)(s => run(p1)(s) == run(p2)(s))

        def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
      }

    }

    object Hei {

      val v = "pelle"
      val v2: Parser[String] = "hei"

      val p: Parser[Int] = ???
      val p2: ParserOps[Int] = p
      p2.|(p)


      p | p

      val v3 = asStringParser(42)(i => "hei")
    }

  }

  object Ex1 {
    // Implemented map2 using product and many1 using map2
  }

  def main(args: Array[String]): Unit = {
    println("pelle")

  }

}
