package no.jergan.scrapbook.fpinscala

object Chapter9 {

  trait Parsers[ParseError, Parser[+_]] {

    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
      def |[B>:A](p2: Parser[B]): Parser[B] = Parsers.this.or(p, p2)
      def or[B>:A](p2: => Parser[B]): Parser[B] = Parsers.this.or(p, p2)
    }

  }


  def main(args: Array[String]): Unit = {
    println("pelle")

  }

}
