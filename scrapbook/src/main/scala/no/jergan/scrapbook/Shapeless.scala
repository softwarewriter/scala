package no.jergan.scrapbook

import shapeless.Generic.Aux
import shapeless.{::, lens, Generic, HList, HNil, Poly1}

object Shapeless {

  def main(args: Array[String]): Unit = {

    case class Person(name: String, age: Int, mother: Option[Person])

    val pelle = Person("hei", 42, None)

    val l: List[String] = List()
    val l1: HList       = HNil

    val l2: Int :: String :: Person :: HNil = 42 :: "ole" :: pelle :: HNil

    object print extends Poly1 {

      implicit def caseInt: Case[Int] { type Result } = at[Int] { i =>
        println(s"int: " + i)
      }

      implicit def caseString = at[String] { s =>
        println(s"string: " + s)
      }

      implicit def casePelle = at[Person] { p =>
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

    val ageLens  = lens[Person].age
    val age: Int = ageLens.get(pelle)

  }
}
