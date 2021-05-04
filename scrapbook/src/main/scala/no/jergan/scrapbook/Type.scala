package no.jergan.scrapbook

object Type {

  type Result[A] = Either[Int, A]

  def m1(): Unit = {

    var x: Any = ???

    x = "pelle"
//    val s: String = x

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
