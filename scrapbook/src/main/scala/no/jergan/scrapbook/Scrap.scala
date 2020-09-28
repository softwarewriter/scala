package no.jergan.scrapbook

import no.jergan.scrapbook.Scrap.a

/**
 * What does this class do?
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */

object Scrap extends App {

  val list: IndexedSeq[Int] = for (i <- 0 until 20 if (i % 2 == 0)) yield i
  println(list.size)


  val a = 3;

  val block: Int = {
     println(a)
    4
  }
  println(block)

  def m(a: Int) = a * a

   println(m(4))
   println(m(block))

  val sq: Int => Int = (x: Int) => x * x


  def m3(f: Int => Int) = f(3)


  println(m3(sq))

}
