package no.jergan.scrapbook

/**
  * What does this class do?
  *
  * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
  */
object Mixin2 {

  trait A {
    def a: String
  }

  trait B extends A {
    override def a: String = "b"
  }

  trait C extends A {
    override def a: String = "c"
  }

  class D extends C with B {
//    override def a: String = "d"
  }

  def main(args: Array[String]): Unit = {

    def m1(implicit d: D): Unit = {
      m2
    }

    def m2(implicit a: A): Unit = println(a.a)

    implicit val d: D = new D

    m1

    val bd: BigDecimal = BigDecimal(42)
    val ul: java.math.BigDecimal = bd.underlying()

  }


}
