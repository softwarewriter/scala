package no.jergan.scrapbook


/**
  * What does this class do?
  *
  * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
  */
object Mixin {

  trait A {
    b: B =>
    def as = s"a ${b.bs}"
  }

  trait B {
    def bs = "b"
  }

  trait C {
    def cs = "c"
  }

  def main(args: Array[String]): Unit = {
    val a: A = new A with B with C
    println(a.as)
  }

}
