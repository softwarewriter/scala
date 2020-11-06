package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter5.Stream

/**
 * First chapter done according to new version of book.
 */
object Chapter6 {


  trait RNG {
    def nextInt: (Int, RNG)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {

    val (a, nextRng) = rng.nextInt




  }

  object Ex1 {
    def test(): Unit = {
      println("pelle2")
    }
  }

  def main(args: Array[String]): Unit = {
    Ex1.test()
  }

}
