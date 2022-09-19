package no.jergan.scrapbook

import scala.util.Try


/**
 * Cats.
 */
object ScalaTry {
  def main(args: Array[String]): Unit = {
    val t: Try[Int] = Try{
      println("getting a value")
      42
    }
    t.toEither.fold(e => println(e.getMessage), i => println(i))
    t.map(i => i + 1).toEither.fold(e => println(e.getMessage), i => println(i))
  }

}
