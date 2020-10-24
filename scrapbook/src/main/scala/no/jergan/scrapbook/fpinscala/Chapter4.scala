package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter3.{Cons, Liste, Nil}
import org.http4s.Uri


object Chapter4 {

  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = {
      this match {
        case Some(value) => Some(f(value))
        case None => None
      }
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      map(f).getOrElse(None)
    }

    def getOrElse[B >: A](default: => B): B = {
      this match {
        case Some(value) => value
        case None => default
      }
    }

    // TODO: How can this be implemented without match?
    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      this match {
        case Some(value) => Some(value)
        case None => ob
      }
    }

    def filter(f: A => Boolean): Option[A] = {
      flatMap(value => if (f(value)) Some(value) else None)
    }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = {
      this match {
        case Right(a) => Right(f(a))
        case Left(e) => Left(e)
      }
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      this match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
      this match {
        case Right(a) => Right(a)
        case Left(e) => b
      }
    }

    def orElseAllErrors[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
      this match {
        case Right(a) => Right(a)
        case Left(ea) => {
          b match {
            case Right(b) => Right(b)
            case Left(eb) => Left(ea, eb)
          }
        }
      }
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      this
        .flatMap(ra => {
          b.map(rb => f(ra, rb))
      })
    }

  }

  case class Left[+E](values: E*) extends Either[E, Nothing]
  case class Right[+A](values: A) extends Either[Nothing, A]

  object Ex1 {

    def test(): Unit = {
      class A1
      class A2 extends A1
      class A3 extends A2

      def elseProvider: A2 = {
        println("evaluate else1")
        new A2
      }

      Some(new A2).getOrElse(elseProvider)
      None.getOrElse(elseProvider)

      def elseProvider2: Option[A2] = {
        println("evaluate else2")
        Some(new A2)
      }
      Some(new A2).orElse(elseProvider2)
      None.orElse(elseProvider2)

      println(None.orElse(None).orElse(None).orElse(Some(new A2)))

    }

  }

  object Ex2 {

    def mean(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) None else Some(xs.sum / xs.size)
    }

    def variance(xs: Seq[Double]): Option[Double] = {
      mean(xs)
        .flatMap(m => if (xs.size < 2) None else Some(xs.map(x => math.pow(x - m, 2)).sum / (xs.size - 1)))
    }
  }

  object Ex3 {
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      a
        .flatMap(aa => b
          .map(bb => f(aa, bb)))
    }

    def test(): Unit = {
      println(map2(Some("a"), Some("b"))((a: String, b: String) => a + " and " + b))
    }
  }

  object Ex4 {
    import java.util.regex._
    def pattern(s: String): Option[Pattern] =
      try {
        Some(Pattern.compile(s)) }
      catch {
        case e: PatternSyntaxException => None
      }

    def mkMatcher(pat: String): Option[String => Boolean] = pattern(pat) map (p => (s: String) => p.matcher(s).matches)

    def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = {
      Ex3.map2(mkMatcher(pat1), mkMatcher(pat2)) ((a, b) => a(s) && b(s))
    }

    def test(): Unit = {
      println(bothMatch_2("ole", "dole", "ole og dole"))
    }
  }

  object Ex5 {
    def sequence[A](l: List[Option[A]]): Option[List[A]] = {
      // None if at least one is None
      val as: List[A] = l
        .collect { case sa: Some[A] => sa.get }
      if (l.size == as.size) Some(as) else None
    }

    def test(): Unit = {
      println(sequence(List(Some(1), Some(2))))
      println(sequence(List(Some(1), None, Some(2))))
    }
  }

  // TODO: Do we have to use two match statements on Option?
  object Ex6 {
    def traverse[A, B](as: Liste[A])(f: A => Option[B]): Option[Liste[B]] = {
      as match {
        case Cons(h, t) => {
          f(h) match {
            case Some(b) => {
              traverse(t)(f) match {
                case Some(l) => Some(Cons(b, l))
                case None => None
              }
            }
            case None => None
          }
        }
        case Nil => Some(Nil)
      }
    }

    def sequenceUsingTraverse[A](l: Liste[Option[A]]): Option[Liste[A]] = {
      traverse(l)(a => a)
    }

    def test(): Unit = {
      val l: Liste[Int] = Chapter3.apply(1, 2, 3)
      println(traverse(l)(a => Some(a)))
      println(traverse(l)(a => if (a % 2 == 0) Some(a) else None))
      println(traverse(l)(a => if (a % 2 == 1) Some(a) else None))

      println(sequenceUsingTraverse(Chapter3.apply(Some(1), Some(2))))
      println(sequenceUsingTraverse(Chapter3.apply(Some(1), None, Some(2))))
    }
  }

  object Ex7 {

    def test(): Unit = {

      println(Right("hei").map(a => a + a))
      val result: Either[String, (Nothing, Int, Double)] = for {
        age <- Right(42)
        name <- Left("invalid name")
        salary <- Right(1000000.0)
      } yield (name, age, salary)

    }
  }

  object Ex8 {

    // TODO: Do we have to use two match statements on Either?
    def sequence[E, A](l: Liste[Either[E, A]]): Either[E, Liste[A]] = {
      l match {
        case Cons(head, tail) => head match {
          case Right(r1) => {
            sequence(tail) match {
              case Right(r2) => Right(Cons(r1, r2))
              case Left(l2) =>Left(l2)
            }
          }
          case Left(l1) => Left(l1)
        }
        case Nil => Right(Nil)
      }
    }

    // TODO: Do we have to use two match statements on Either?
    def traverse[E, A, B](as: Liste[A])(f: A => Either[E, B]): Either[E, Liste[B]] = {
      as match {
        case Cons(head, tail) => {
          f(head) match {
            case Right(b) => {
              traverse(tail)(f) match {
                case Right(r2) => Right(Cons(b, r2))
                case Left(l2) => Left(l2)
              }
            }
            case Left(l1) => Left(l1)
          }
        }
        case Nil => Right(Nil)
      }
    }

    def test(): Unit = {

      println(sequence(Chapter3.apply(Right("ole"), Right("dole"), Left("doff"))))
      println(sequence(Chapter3.apply(Right("ole"), Right("dole"), Right("doff"))))

      val l: Liste[Int] = Chapter3.apply(1, 2, 3)
      println(traverse(l)(a => Right(a)))
      println(traverse(l)(a => if (a % 2 == 0) Right(a) else Left(a + " is not even")))
      println(traverse(l)(a => if (a % 2 == 1) Right(a) else Left(a + " is not odd")))
    }
  }

  object Ex9 {

    def sequenceAllErrors[E, A](l: Liste[Either[E, A]]): Either[E, Liste[A]] = {
      l match {
        case Cons(head, tail) => head match {
          case Right(r1) => {
            sequenceAllErrors(tail) match {
              case Right(r2) => Right(Cons(r1, r2))
              case Left(l2) =>Left(l2)
            }
          }
          case Left(l1) => Left(l1)
        }
        case Nil => Right(Nil)
      }
    }

    def traverseAllErrors[E, A, B](as: Liste[A])(f: A => Either[E, B]): Either[E, Liste[B]] = ???

    def test(): Unit = {

      println(sequenceAllErrors(Chapter3.apply(Right("ole"), Right("dole"), Left("doff"), Left("doffen"))))
      println(sequenceAllErrors(Chapter3.apply(Right("ole"), Right("dole"), Right("doff"), Right("doffen"))))

      val l: Liste[Int] = Chapter3.apply(1, 2, 3, 4)
      println(traverseAllErrors(l)(a => Right(a)))
      println(traverseAllErrors(l)(a => if (a % 2 == 0) Right(a) else Left(a + " is not even")))
      println(traverseAllErrors(l)(a => if (a % 2 == 1) Right(a) else Left(a + " is not odd")))
    }
  }

  def main(args: Array[String]): Unit = {
    Ex9.test()
  }

}
