package no.jergan.scrapbook

import no.jergan.scrapbook.Scrap2.O3.A
import no.jergan.scrapbook.Scrap2.O3.O4.me

object Implicit {

  case class A(name: String, b: B)
  case class B(name: String)

  trait ToString[T] {
    def s(t: T): String
  }

  def main(args: Array[String]): Unit = {

    // Order of implicits matter -> Compilation error.

    implicit val toStringB: ToString[B] = b => s"name: ${b.name}"

    implicit val toStringA: ToString[A] = {
      a => {
        val ev = implicitly[ToString[B]]
        s"name: ${a.name}, ${ev.s(a.b)}"
      }
    }

    def print[T](a: T)(implicit ev: ToString[T]): Unit = {
      println(ev.s(a))
    }

    val b = B("nameb")
    val a = A("name1", b)

    println("pelle1")
    print(b)
    print(a)

    println("pelle")
  }


}
