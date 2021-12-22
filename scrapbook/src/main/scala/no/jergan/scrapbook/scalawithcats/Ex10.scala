package no.jergan.scrapbook.scalawithcats

import cats.implicits.catsSyntaxTuple2Semigroupal
import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import no.jergan.scrapbook.scalawithcats.Ex10.Check.{And, Or, Pure}


object Ex10 {

  class Pelle()

  sealed trait Check[E, A] {

    def and(check: Check[E, A]): And[E, A] = And(this, check)

    def apply(a: A)(implicit ev: Semigroup[E]): Validated[E, A] = this match {
      case Pure(f) => f(a)
      case And(l, r) => (l(a), r(a)).mapN((v1, v2) => a)
      case Or(l, r) => (l(a), r(a)) match {
        case (Valid(a1), _) => Valid(a1)
        case (_, Valid(a2)) => Valid(a2)
        case (Invalid(e1), Invalid(e2)) => Invalid(ev.combine(e1, e2))
      }

        (l(a), r(a)).mapN((v1, v2) => a)
    }
  }

  object Check {
    case class Pure[E, A](f: A => Validated[E, A]) extends Check[E, A]
    case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]
    case class Or[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]
  }

  def maxLengthCheck(maxLength: Int) = Pure[List[String], String](a => if (a.length > maxLength) Invalid(List(s"$a can be of maximum length $maxLength, was ${a.length}"))
      else Valid(a))

  val alphaCheck = Pure[List[String], String](a => if ("""^[a-zA-Z]*$""".r.findFirstMatchIn(a).isEmpty) Invalid(List(s"Must contain only alpha characters, was $a"))
      else Valid(a))


  def main(args: Array[String]): Unit = {

    val c1 = maxLengthCheck(4)
    val c2 = alphaCheck
    val c3 = c1.and(c2)

    /*
    println(c1.apply("hei"))
    println(c1.apply("heihei"))
    println(c2.apply("hei"))
    println(c2.apply("he1"))

     */

    println(c3("heihei42"))
  }

}
