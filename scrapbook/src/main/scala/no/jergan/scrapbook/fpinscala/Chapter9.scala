package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter8.Prop.forAll
import no.jergan.scrapbook.fpinscala.Chapter8.{Gen, Prop}

import scala.::
import scala.util.matching.Regex

object Chapter9 {

  trait Parsers[ParseError, Parser[+_]] {

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

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

    object J {

      def root(): Parser[JSON.JObject] = {
        jsonMap()
      }

      def json(): Parser[JSON] = {
        jsonNull or jsonString or jsonNumber or jsonBool or json or jsonArray or jsonMap
      }

      def jsonNull(): Parser[JSON] = {
        string("null").map(_ => JSON.JNull)
      }

      def jsonString(): Parser[JSON.JString] = {
        char('"')
          .flatMap(_ => regex("[A-Z].*".r)
            .flatMap(s => char('"').map(_ => JSON.JString(s))))
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
          _ <- jsonWhite()
          _ <- char('[')
          elements <- many[JSON](jsonArrayElement())
          last <- many[JSON](json())
          _ <- jsonWhite()
          _ <- char(']')
        } yield {
          JSON.JArray((elements ++ last).toIndexedSeq)
        }
      }

      def jsonArrayElement(): Parser[JSON] = {
        for {
          _ <- jsonWhite()
          o <- json()
          _ <- jsonWhite()
          _ <- char(',')
          _ <- jsonWhite()
        } yield o
      }

      def jsonMap(): Parser[JSON.JObject] = {
        for {
          _ <- jsonWhite()
          _ <- char('{')
          entries <- many[JSON.JMapEntry](jsonMapEntry())
          _ <- char('}')
          _ <- jsonWhite()
        } yield JSON.JObject(entries.map(e => (e.name, e.value)).toMap)
      }

      def jsonMapEntry(): Parser[JSON.JMapEntry] = {
        for {
          _ <- jsonWhite()
          name <- jsonQString()
          _ <- jsonWhite()
          _ <- char('=')
          _ <- jsonWhite()
          value <- json()
          _ <- jsonWhite()
        } yield JSON.JMapEntry(name, value)
      }

      def jsonWhite(): Parser[JSON] = {
        regex("\\s*".r).map(_ => JSON.JNull)
      }

      def jsonQString(): Parser[String] = {
        char('"')
          .flatMap(_ => regex("[A-Z].*".r)
            .flatMap(s => char('"').map(_ => s)))
      }
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

  def main(args: Array[String]): Unit = {

    def f(a: Boolean) = {
      if (a) true
      false
    }
    println("pelle: " + f(true))

  }

}
