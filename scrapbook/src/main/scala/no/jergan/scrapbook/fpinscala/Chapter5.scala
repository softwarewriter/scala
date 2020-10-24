package no.jergan.scrapbook.fpinscala

object Chapter5 {

  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]

    def isEmpty: Boolean = uncons.isEmpty
  }

  object Stream {
    def empty[A]: Stream[A] =
      new Stream[A] {
        def uncons = None
      }

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      new Stream[A] {
        lazy val uncons = Some((hd, tl))
      }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  object Ex1 {

    def test(): Unit = {

      val l = List(1, 2, 3)

      val block: Int = {
        println("block")
        1
      }

      def m(): Int = {
        println("m")
        1
      }

      def requiredInt(i: => Int) = {

        lazy val j  = i;
//        println(j * j)
      }

      requiredInt(m)

    }
  }

  def main(args: Array[String]): Unit = {

    Ex1.test()

  }

}
