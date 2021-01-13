package no.jergan.scrapbook.fpinscala

object ContextBound {

  trait Pelle[F] {
    def p(): F
  }

  object Pelle {
    def apply[F](implicit ev: Pelle[F]): Pelle[F] = ev
  }

  trait Ole[F] {
    def o(): F
  }

  object Ole {
    def apply[F](implicit ev: Ole[F]): Ole[F] = ev
  }

  //  class MyClass[F](implicit val ps: Pelle[String]) {
  class MyClass[F: Pelle: Ole] {

    def m(): String = {
//      val ps = implicitly[Pelle[F]]
      val ps = Pelle[F]
      val os = Ole[F]
      "m" + ps.p() + os.o()
    }
  }

  def main(args: Array[String]): Unit = {

    implicit val pelleString: Pelle[String] = new Pelle[String] {
      override def p(): String = "p"
    }
    implicit val oleString: Ole[String] = new Ole[String] {
      override def o(): String = "o"
    }

    val myClass = new MyClass[String]

    println(myClass.m())
  }

}
