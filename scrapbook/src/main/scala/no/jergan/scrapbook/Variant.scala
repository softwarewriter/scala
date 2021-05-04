package no.jergan.scrapbook

object Variant {

  class A1
  class A2 extends A1
  class A3 extends A2

  {
    trait Invariant[A2]

    val value: Invariant[A2] = ???
//    val a1: Invariant[A1] = value
    val a2: Invariant[A2] = value
//    val a3: Invariant[A3] = value
  }

  {
    trait Covariant[+A2]

    val value: Covariant[A2] = ???
    val a1: Covariant[A1]    = value
    val a2: Covariant[A2]    = value
//    val a3: Covariant[A3] = value
  }

  {
    trait Contravariant[-A2]

    val value: Contravariant[A2] = ???
//    val a1: ContraVariant[A1] = value
    val a2: Contravariant[A2] = value
    val a3: Contravariant[A3] = value
  }

}
