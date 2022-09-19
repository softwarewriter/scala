package no.jergan.scrapbook

import shapeless.Generic.Aux
import shapeless.{::, Generic, HList, HNil, Lens, OpticDefns, Poly1, lens}

object Shapeless {
/*
  case class Pair[A, B](a: A, b: B)

  val p1 = Pair("pelle", 42)
  val p2: Pair[String, Int] = ???
  val p3: Long Pair String = ???
  val p4: Long Pair String Pair Int = ???

 */

  def main(args: Array[String]): Unit = {

    case class Person(name: String, age: Int, mother: Option[Person])

    val pelle = Person("hei", 42, None)

    val l2: Int :: String :: Person :: HNil = 42 :: "ole" :: pelle :: HNil
    val l3: ::[Int, ::[String, ::[Person, HNil]]] = 42 :: "ole" :: pelle :: HNil

    object print extends Poly1 {

      implicit def caseInt: Case[Int] { type Result } = at[Int] { i =>
        println(s"int: " + i)
      }

      implicit def caseString: Case[String] { type Result } = at[String] { s =>
        println(s"string: " + s)
      }

      implicit def casePelle: Case[Person] { type Result } = at[Person] { p =>
        println(s"pelle: " + p.name)
      }
    }

    //  val s: String = l(1)

//    l2.head
//    l2(1)

    println(l2)

    l2.map(print)

    val gen: Aux[Person, String :: Int :: Option[Person] :: HNil] = Generic[Person]
    val pl: String :: Int :: Option[Person] :: HNil               = gen.to(pelle)

    val personLens: OpticDefns.RootLens[Person] = lens[Person]
    val ageLens: Lens[Person, Int] = personLens.age
    val age: Int = ageLens.get(pelle)

  }
}
