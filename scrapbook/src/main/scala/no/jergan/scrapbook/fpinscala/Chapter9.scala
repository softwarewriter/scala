package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter6.State
import no.jergan.scrapbook.fpinscala.Chapter8.Prop.forAll
import no.jergan.scrapbook.fpinscala.Chapter8.{Gen, Prop}

import scala.util.matching.Regex

object Chapter9 {

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
    lazy val col = input.slice(0,offset+1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart }

    def current(): String = {
      input.substring(offset)
    }

    def jump(n: Int): Location = {
      Location(input, offset + n)
    }

    def substring(n: Int): String = {
      input.substring(offset, offset + n)
    }

    def toError(message: String): ParseError = {
      ParseError(List((this, message)))
    }

  }

  case class ParseError(stack: List[(Location, String)]) {
    def push(location: Location, message: String): ParseError = copy(stack = (location, message) :: stack)
  }

  trait Parsers[Parser[+_]] {

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    def errorLocation(e: ParseError): Location

    def errorMessage(e: ParseError): String

    // primitives

    implicit def string(s: String): Parser[String]

    def succeed[A](a: A): Parser[A] = {
      string("").map(_ => a)
    }

    def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

    def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

    def slice[A](p: Parser[A]): Parser[String]

    def regex(r: Regex): Parser[String]

    // combined

    def map[A, B](a: Parser[A])(f: A => B): Parser[B] = {
      a.flatMap(aa => succeed(f(aa)))
    }

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
      if (n == 0) succeed(List())
      else map2(p, listOfN(n - 1, p))(_ :: _)
    }

    def many[A](p: Parser[A]): Parser[List[A]] = {
      map2(p, many(p))((a, b) => a :: b) or succeed(List())
    }

    def many1[A](p: Parser[A]): Parser[List[A]] = {
      map2(p, many(p))(_ :: _)
    }

    def product[A, B](a: Parser[A], b: Parser[B]): Parser[(A, B)] = {
      a.flatMap(aa => b.map(bb => (aa, bb)))
    }

    def map2[A, B, C](a: Parser[A], b: Parser[B])(f: (A, B) => C): Parser[C] = {
      map(product(a, b))(f.tupled)
    }

    def map2UsingFlatMap[A, B, C](a: Parser[A], b: Parser[B])(f: (A, B) => C): Parser[C] = {
      a.flatMap(aa => b.map(bb => f(aa, bb)))
    }

    def char(c: Char): Parser[Char] = {
      string(c.toString).map(_.charAt(0))
    }

    def composed(): Parser[Int] = {
     /*
      regex("[0-9+]".r)
        .flatMap(s => listOfN(s.toInt, char('a'))
        .map(ss => ss.toString()))
      */
      for {
        digit <- regex("[0-9]+".r)
        n = digit.toInt
        // we really should catch exceptions thrown by toInt // and convert to parse failure
        _ <- listOfN(n, char('a'))
      } yield n
    }

    // Error
    def expected[A](expected: String)(a: Parser[A]): Parser[A] = {
      a
    }

    def error[A](error: String)(a: Parser[A]): Parser[A] = {
      a
    }

    def scope[A](msg: String)(p: Parser[A]): Parser[A]

    object J {

      def root(): Parser[JSON] = {
        json()
      }

      def json(): Parser[JSON] = {
        jsonNull or jsonString or jsonNumber or jsonBool or json or jsonArray or jsonMap
      }

      def jsonNull(): Parser[JSON] = {
        string("null").map(_ => JSON.JNull)
      }

      def jsonString: Parser[JSON.JString] = {
        jsonInside('"', '"', regex("[A-Z].*".r).map(s => JSON.JString(s)))
      }

      def jsonNumber(): Parser[JSON.JNumber] = {
        regex("[0-9].*".r)
          .map(s => JSON.JNumber(Integer.parseInt(s)))
      }

      def jsonBool(): Parser[JSON.JBool] = {
        or(regex("true".r), regex("false".r))
          .map(s => JSON.JBool(s.toBoolean))
      }

      def jsonArray(): Parser[JSON.JArray] = {
        for {
          _ <- jsonWhite
          _ <- char('[')
          elements <- many[JSON](jsonArrayElement())
          last <- many[JSON](json())
          _ <- jsonWhite
          _ <- char(']')
        } yield {
          JSON.JArray((elements ++ last).toIndexedSeq)
        }
      }

      def jsonArrayElement(): Parser[JSON] = {
        for {
          _ <- jsonWhite
          o <- json()
          _ <- jsonWhite
          _ <- char(',')
          _ <- jsonWhite
        } yield o
      }

      def jsonMap(): Parser[JSON.JObject] = {
        for {
          _ <- jsonWhite
          _ <- char('{')
          entries <- many[JSON.JMapEntry](jsonMapEntry())
          _ <- char('}')
          _ <- jsonWhite
        } yield JSON.JObject(entries.map(e => (e.name, e.value)).toMap)
      }

      def jsonMapEntry(): Parser[JSON.JMapEntry] = {
        for {
          _ <- jsonWhite
          name <- jsonString
          _ <- jsonWhite
          _ <- char('=')
          _ <- jsonWhite
          value <- json()
          _ <- jsonWhite
        } yield JSON.JMapEntry(name.get, value)
      }

      def jsonWhite: Parser[JSON] = {
        regex("\\s*".r).map(_ => JSON.JNull)
      }
    }

    def jsonInside[A](start: Char, end: Char, parser: Parser[A]): Parser[A] = {
      char(start)
        .flatMap(_ => parser)
          .flatMap(json => char(end).map(_ => json))
    }

    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

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

    trait JSON

    object JSON {
      case object JNull extends JSON
      case class JNumber(get: Double) extends JSON
      case class JString(get: String) extends JSON
      case class JBool(get: Boolean) extends JSON
      case class JArray(get: IndexedSeq[JSON]) extends JSON
      case class JObject(get: Map[String, JSON]) extends JSON
      case class JMapEntry(name: String, value: JSON) extends JSON
    }
  }

//  type Parser[+A] = Location => Result[A]

  trait Result[+A] {

    def mapSuccess[B](f: Success[A] => Success[B]): Result[B] = {
      this match {
        case Success(a, charsConsumed) => f(Success(a, charsConsumed))
        case Failure(pe) => Failure(pe)
      }
    }

    def mapFailure(f: ParseError => ParseError): Result[A] = {
      this match {
        case Failure(pe) => Failure(f(pe))
        case _ => this
      }
    }
  }

  case class Success[+A](a: A, charsConsumed: Int) extends Result[A]
  case class Failure(pe: ParseError) extends Result[Nothing]

  abstract class MyParser[+A]() {

    def parse(input: Location): Result[A]

    // alternativt
    // def run(s: State[Location, Either[ParseError, A]])
  }

  object MyParsers extends Parsers[MyParser] {

    override def run[A](p: MyParser[A])(input: String): Either[ParseError, A] = {
      val initialLocation = Location(input, 0)
      val stack: List[(Location, String)] = List.empty
      p.parse(initialLocation) match {
        case Failure(pe) => Left(pe)
        case Success(a, _) => Right(a)
      }
    }

    override def errorLocation(e: ParseError): Location = ???

    override def errorMessage(e: ParseError): String = ???

    override implicit def string(s: String): MyParser[String] = (input: Location) => {
      if (input.current().startsWith(s)) Success(s, s.length) else Failure(input.toError(s"Expected $s"))
    }

    override def or[A](s1: MyParser[A], s2: => MyParser[A]): MyParser[A] = (input: Location) => {
      s1.parse(input) match {
        case Failure(_) => s2.parse(input)
        case Success(a, charsConsumed) => Success(a, charsConsumed)
      }
    }

    override def flatMap[A, B](a: MyParser[A])(f: A => MyParser[B]): MyParser[B] = (input: Location) => {
      a.parse(input) match {
        case Failure(pe) => Failure(pe)
        case Success(a, charsConsumed) => f(a).parse(input.jump(charsConsumed))
      }
    }

    override def slice[A](p: MyParser[A]): MyParser[String] = (input: Location) => {
      p.parse(input).mapSuccess(s => Success(input.substring(s.charsConsumed), s.charsConsumed))
    }

    override def regex(r: Regex): MyParser[String] = (input: Location) => {
      r.findFirstIn(input.current()) match {
        case None => Failure(input.toError(s"expected $r"))
        case Some(s) => Success(s, s.length)
      }
    }

    override def scope[A](message: String)(p: MyParser[A]): MyParser[A] = (input: Location) => {
      p.parse(input).mapFailure(_.push(input, message))
    }
  }


  object Ex1 {
    // Implemented map2 using product and many1 using map2
  }

  object Ex2 {
    // Assosiative
  }

  object Ex3 {
    // Implemented many using map2, or and success.
    // Cheated here, not good.
  }

  object Ex4 {
    // Implemented listOfN using map2 and success

  }

  object Ex5 {
    // Implemented delay signature, which seemed to be tedious to remember to use.
  }

  object Ex6 {
    // Implemented composed
  }

  object Ex7 {
    // Implemented product and map2 using flatMap
  }

  object Ex8 {
    // Implemented map
  }

  object Ex9 {
    // Implemented json functionality
  }

  object Ex10 {
    // Implemented error combinators.
  }

  object Ex11 {
    // Errors in or-chain.
    // - silent - no errors from parser
  }

  object Ex12 {
    // Representation of MyParser.
  }

  object Ex13 {
    // Change parse result type to Result as proposed in text.
    // This is not better that what I did, just different.
  }

  def main(args: Array[String]): Unit = {

    trait T {

      def m(i: Int): Int

    }

    class C extends T {
      override def m(i: Int): Int = i * 2
    }

    case class CC() extends T {
      override def m(i: Int): Int = i * 2
    }

    object O extends T {
      override def m(i: Int): Int = i * 3
    }

    case object CO extends T {
      override def m(i: Int): Int = i * 3
    }

    println(new C().m(3))
    println(CC().m(3))

    println(O.m(3))
    println(CO.m(3))


  }

}
