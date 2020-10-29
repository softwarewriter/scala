package no.jergan.scrapbook.fpinscala

object Chapter5 {

  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]

    def toList: List[A]

    def take(n: Int): Stream[A]

    def isEmpty: Boolean = uncons.isEmpty

  }

  object Stream {
    def empty[A]: Stream[A] =
      new Stream[A] {
        def uncons = None

        override def toList: List[A] = List.empty

        override def take(n: Int): Stream[A] = empty

      }

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      new Stream[A] {
        lazy val uncons = Some((hd, tl))

        override def toList: List[A] = uncons match {
            case Some((hd, tl)) => List(hd) ++ tl.toList
          }

        override def take(n: Int): Stream[A] =
          if (n == 0) empty else
            uncons match {
              case Some((hd, tl)) => cons(hd, tl.take(n - 1))
            }

        def takeWhile(p: A => Boolean): Stream[A] = ???
      }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  object Ex1 {

    def test(): Unit = {
      println(Stream(1, 2, 3).toList)
    }
  }

  object Ex2 {

    def test(): Unit = {
     println(Stream(1, 2, 3).take(2).toList)
    }

  }

  def main(args: Array[String]): Unit = {

    Ex2.test()

  }

}
