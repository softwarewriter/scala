package no.jergan.scrapbook

import cats.{Applicative, Defer, Eval}
import cats.data.{OptionT, ReaderT}

object Implicit {

  object ImplicitValue {

    case class A(name: String, b: B)

    object A {
      implicit val toStringA: ToString[A] = {
        a => {
          val ev = implicitly[ToString[B]]
          s"name: ${a.name}, ${ev.s(a.b)}"
        }
      }

    }

    case class B(name: String)

    object B {
      implicit val toStringB: ToString[B] = b => s"name: ${b.name}"
    }

    trait ToString[T] {
      def s(t: T): String
    }

    def invoke(): Unit = {

      // Order of implicits matter -> Compilation error (forward reference extends over definition of value toStringA)
      // However, implicit definitions can be put in companion object as well: https://docs.scala-lang.org/tour/implicit-parameters.html
      // Here "in all the companion objects associated with the implicit candidate type" mean that
      // ToString[B] must be defined in companion object of B.

      //implicit val toStringB: ToString[B] = b => s"name: ${b.name}"

      /*
      implicit val toStringA: ToString[A] = {
        a => {
          val ev = implicitly[ToString[B]]
          s"name: ${a.name}, ${ev.s(a.b)}"
        }
      }

       */

      def print[T](a: T)(implicit ev: ToString[T]): Unit = {
      println(ev.s(a))
    }

      val b = B("nameb")
      val a = A("name1", b)

      println("pelle1")
      print(b)
      print(a)

    }


  }

  object ImplicitConversion {

    implicit def convert: String => MyType = s => MyType(s)

    case class MyType(value: String)

    def takeIt(it: MyType): Unit = {
      println(s"MyType had value ${it.value}")
    }

    def invokeTakeIt(): Unit = {
      takeIt("pelle conversion")
    }

  }

  object ImplicitClass {

    implicit class MyTypeClass(val value: String)

    def takeIt(it: MyTypeClass): Unit = {
      println(s"MyType had value ${it.value}")
    }

    def invokeTakeIt(): Unit = {

      takeIt("pelle class")
    }

  }
  def main(args: Array[String]): Unit = {

    val set: Iterable[String] = Set("a", "b")
    val seq = Seq.apply("a", "b")
    seq.foreach(a => ())
    println(seq.mkString(", "))
    println(seq.getClass)
    ImplicitValue.invoke()
    ImplicitConversion.invokeTakeIt()
    ImplicitClass.invokeTakeIt()
  }


}
