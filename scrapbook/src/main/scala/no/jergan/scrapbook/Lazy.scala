package no.jergan.scrapbook

object Lazy {

  object ByValueOrName {
    def byValue(a: String): Unit = {
      println("byValue")
      println(a)
      println(a)
    }

    def byName(a: => String): Unit = {
      println("byName")
      println(a)
      println(a)
    }

    def test: Unit = {
      byValue({ println("printByValue"); "value"})
      byName({ println("printByName"); "name"})
    }
  }

  object Lazy {

    def m(s: => String): Unit = {
      println("m")
      println(s)
      println(s)
    }

    val s1 = {println("printS1"); "s1"}
    lazy val s2 = {println("printS2"); "s2"}

    def test: Unit = {
      m(s1)
      m(s2)
    }
  }

  def main(args: Array[String]): Unit = {
    ByValueOrName.test
    Lazy.test
  }

}
