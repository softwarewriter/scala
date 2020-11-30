package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter8.Prop.forAll
import no.jergan.scrapbook.fpinscala.Chapter8.{Gen, Prop}

object Chapter9 {

  trait Parsers[ParseError, Parser[+_]] {

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    def char(c: Char): Parser[Char]

    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

    def many[A](p: Parser[A]): Parser[List[A]]

    def map[A, B](a: Parser[A])(f: A => B): Parser[B]

    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
      def |[B>:A](p2: Parser[B]): Parser[B] = Parsers.this.or(p, p2)
      def or[B>:A](p2: => Parser[B]): Parser[B] = Parsers.this.or(p, p2)
      def many: Parser[List[A]] = Parsers.this.many(p)
      def map[B](f: A => B): Parser[B] = Parsers.this.map(p)(f)

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

//    import Chapter9.Parsers

//    val p: Parsers[String, List[_]] = ???

//    import p._

//    val v = "ole" | "dole"
  }


  def main(args: Array[String]): Unit = {
    println("pelle")

  }

}
