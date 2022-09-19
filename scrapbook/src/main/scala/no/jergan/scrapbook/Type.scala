package no.jergan.scrapbook

import cats.data.{Kleisli, OptionT}
import cats.{Applicative, Monad}
import org.http4s.CharsetRange.*
import org.http4s.{Request, Response}
import org.http4s.server.Middleware

object SomeOtherObject {

  // Type imports are grey (like unused) in IntelliJ
  import Type.Result

  type MyType = Result[String]

}


object Type {

  type Result[A] = Either[Int, A]

  def m1(): Unit = {

    var x: Any = ???

    x = "pelle"
//    val s: String = x

    case class S1(s: String)

    type S2 = S1

    object S2 {
      def apply(s: String): S2 = S1(s)
    }

    def m2: Unit = {

      S1("hei")
      S2("hei")
    }
  }

  trait WithType[A] {
    type T
    def get(a: A): T
  }

  class WithString extends WithType[Int] {
    override type T = String
    override def get(int: Int): String = int.toString
  }

  def main(args: Array[String]): Unit = {
    println(new WithString().get(42))
    println(new WithString().getClass)
  }

}
