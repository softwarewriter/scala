package no.jergan.scrapbook.fpinscala

object Chapter10 {

  trait Monoid[A] {

    def op(a1: A, a2: A): A
    val zero: A

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

}
