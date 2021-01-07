package no.jergan.scrapbook

object ScrapType {

  def main(args: Array[String]): Unit = {


    class C[TP] {

      type TF

      def get1: TP = ???

      def get2: TF = ???

    }

    type T = C[_]

    val c = new C()

    type TT = c.TF

    val a: Any = {}
    println(a.getClass)
    println(a)

    val c2: TT = c.get2



  }



}
