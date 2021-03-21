package no.jergan.scrapbook.fpinscala

object Type {

  type Result[A] = Either[Int, A]

  def main(args: Array[String]): Unit = {

    def mA[A](r: Result[A]): Unit = ???

    def mString(r: Result[String]): Unit = ???

    mA(Right("hei"))
    mString(Right("hei"))
    mString(Left(42))

    println("pelle")

  }


}
