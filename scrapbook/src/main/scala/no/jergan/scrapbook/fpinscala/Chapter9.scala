package no.jergan.scrapbook.fpinscala

object Chapter9 {

  trait Parsers[ParseError, Parser[+_]] {

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
      def |[B>:A](p2: Parser[B]): Parser[B] = Parsers.this.or(p, p2)
      def or[B>:A](p2: => Parser[B]): Parser[B] = Parsers.this.or(p, p2)
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
