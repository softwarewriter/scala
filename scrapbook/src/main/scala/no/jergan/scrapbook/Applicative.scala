package no.jergan.scrapbook

trait SemiGroup[A] {
   def append(a1: A, a2: A): A
}

object SemiGroup {

   implicit def ListSemigroup[String]: SemiGroup[List[String]] = new SemiGroup[List[String]] {
      def append(a1: List[String], a2: List[String]) = a1 ::: a2
   }

   /*
   implicit def ListSemigroup[A]: SemiGroup[List[A]] = new SemiGroup[List[A]] {
      def append(a1: List[A], a2: List[A]) = a1 ::: a2
   }

    */
}

sealed trait Validation[E, X] {

   def map[Y](f: X => Y): Validation[E, Y] = this match {
      case Failure(e) => Failure(e)
      case Success(x) => Success(f(x))
   }

   def |@| [Y](f: Validation[E, X => Y])(implicit s: SemiGroup[E]): Validation[E, Y] = (this, f) match {
      case (Failure(e1), Failure(e2)) => Failure(s append(e1, e2))
      case (Failure(e1), Success(_)) => Failure(e1)
      case (Success(_), Failure(e2)) => Failure(e2)
      case (Success(x), Success(k)) => Success(k(x))
   }
}

final case class Failure[E, X](e: E) extends Validation[E, X]
final case class Success[E, X](x: X) extends Validation[E, X]//

// Validators
//
object Validators {

   def validAge(a: Int): Validation[List[String], Int] =
      if (a < 0)
         Failure(List("Age must be greater than 0"))
      else if (a > 130)
         Failure(List("Age must be less than 130"))
      else
         Success(a)

   def validName(s: String): Validation[List[String], String] =
      if (s.headOption exists (_.isUpper))
         Success(s)
      else
         Failure(List("Name must begin with a capital letter"))

   def validPostcode(s: String): Validation[List[String], String] =
      if (s.length == 4 && s.forall(_.isDigit))
         Success(s)
      else
         Failure(List("Postcode must be 4 digits"))
}

object ApplicativeDemo extends App {

   import Validators._

   case class Person(name: String, age: Int, postCode: String)

   def constructor(name: String)(age: Int)(postCode: String) = Person(name, age, postCode)

   val createPerson2: String => Int => String => Person = (Person(_, _, _)).curried
   val createPerson: String => Int => String => Person = constructor
   val age: Validation[List[String], Int] = validAge(42)
   val name: Validation[List[String], String] = validName("Kaare")
   val postcode: Validation[List[String], String] = validPostcode("0854")

   name |@|(age |@|(postcode map createPerson)) match {
      case Success(p) => println("We have a person: " + p)
      case Failure(e) => e foreach println
   }
}