package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter5.Stream

/**
 * First chapter done according to new version of book.
 */
object Chapter6 {


  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (a, nextRng) = rng.nextInt
    a match {
      case (Int.MinValue) => nonNegativeInt(nextRng)
      case _ => if (a >= 0) (a, nextRng) else (a.abs, nextRng)
    }
  }

  object Ex1 {
    def test(): Unit = {

      val rng = SimpleRNG(1)
      println(nonNegativeInt(rng))
    }
  }

  def main(args: Array[String]): Unit = {
    Ex1.test()
  }

}
