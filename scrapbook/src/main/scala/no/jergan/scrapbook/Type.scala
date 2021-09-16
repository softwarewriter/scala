package no.jergan.scrapbook

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

  def main(args: Array[String]): Unit = {

    def mA[A](r: Result[A]): Unit = ???

    def mString(r: Result[String]): Unit = ???

    mA(Right("hei"))
    mString(Right("hei"))
    mString(Left(42))

    println("pelle")

  }

}
