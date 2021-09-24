package no.jergan.scrapbook.scalawithcats

import cats.implicits.toTraverseOps

object Ex7221 {

  def main(args: Array[String]): Unit = {
    val v1 = Vector(1, 2)
    val v2 = Vector(3, 4)
    val list = List(v1, v2)

    val vector = list.sequence
    println(vector)
  }

}
