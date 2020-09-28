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

  val block: Unit = {
    println(a)
    4
    ()
  }
  println(block)

  def m(a: Int) = a * a

  println(m(4))
  //   println(m(block))

  // functions
  def sq1: Int => Int = (x: Int) => x * x
  val sq2: Int => Int = (x: Int) => x * x

  // methods
  def sq3(x: Int): Int = x * x

  //   val sq4(x: Int): Int = x * x

  println(sq1)
  println(sq2)
  //   println(sq3)

  println(sq3(3))

  trait Greeter {
    def greet(name: String): Int
  }

  trait Greeter2 {
    def greet2: String => Int
  }

  class Impl extends Greeter with Greeter2 {

    override def greet(name: String): Int = 42

    override def greet2: String => Int = (s: String) => 43
    //     override def greet: String => Int = (s: String => 42
  }
  val any1: Any    = Int.MaxValue
  val any2: AnyVal = Int.MinValue
  val any3: AnyRef = "hei"
   val any4: AnyRef = (a: String) => a.length

  val anyList: List[Any] = List(any1, any2, any3, any4)
  println(anyList)

  val nul: Null = null

  println(nul)

  val b: Int = null

}
