package no.jergan.scrapbook

/**
 * Functions and methods:
 * http://jim-mcbeath.blogspot.com/2009/05/scala-functions-vs-methods.html
 */
object FunctionAndMethod {

  object method {

    def m(s: String): Int = ???

    def m2(f: String => Int): Int = {
      val ff = f
      42
    }

    def m3(i1: Int)(i2: Int): Int = {
      i1 + i2
    }

    def test(): Unit = {
      val valMHei = m("hei")
//      val valM = m can not assign method to variable
      val valMF = m _ // convert method to a function (eta expansion)
      val valMF2: String => Int = m // convert method to a function (eta expansion)
      m2(m)  // convert method to a function
    }
  }

  object function {

    def f1: String => Int = ???
    val f2 = (s: String) => 42

    val f3: String => Int = (s: String) => 43

    def test(): Unit = {
      val valFHei = f1("hei")
      val valF = f1
      val valF2 = f2
    }
  }

}
