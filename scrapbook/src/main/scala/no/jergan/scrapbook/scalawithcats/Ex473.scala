package no.jergan.scrapbook.scalawithcats

import cats.data.{Writer, WriterT}
import cats.implicits.catsSyntaxApplicativeId

object Ex473 {

  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A): A =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Logged[Int] = {
    val v = if (n == 0) 1.pure[Logged] else factorial(n - 1).map(_ * n)
    v.flatMap(v => Writer(Vector(s"Factorial of $n is $v"), v))
  }

  def main(args: Array[String]): Unit = {
    val ans = factorial(5)
    println(ans.run._2)
    println(ans.run._1)
  }

}
