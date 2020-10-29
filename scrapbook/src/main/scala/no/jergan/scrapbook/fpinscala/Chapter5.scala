package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter5.Stream.empty

object Chapter5 {

  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]

    def toList: List[A]

    def take(n: Int): Stream[A]

    def takeWhile(p: A => Boolean): Stream[A]

    def forAll(p: A => Boolean): Boolean

    def isEmpty: Boolean = uncons.isEmpty

  }

  object Stream {
    def empty[A]: Stream[A] =
      new Stream[A] {
        def uncons = None

        override def toList: List[A] = List.empty

        override def take(n: Int): Stream[A] = empty

        override def takeWhile(p: A => Boolean): Stream[A] = empty

        override def forAll(p: A => Boolean): Boolean = true
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

        override def takeWhile(p: A => Boolean): Stream[A] = {
            uncons match {
              case Some((hd, tl)) => if (p(hd)) cons(hd, tl.takeWhile(p)) else empty
            }
        }

        override def forAll(p: A => Boolean): Boolean = {
          uncons match {
            case Some((hd, tl)) => if (p(hd)) tl.forAll(p) else false
          }

        }
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

  object Ex3 {
    def test(): Unit = {
      println(Stream(1, 2, 3, 4, 5, 6, 7, 8).takeWhile(e => e < 4).toList)
      println(Stream(1, 2, 3, 2, 1).takeWhile(e => e < 4).toList)
    }
  }

  object Ex4 {
    def test(): Unit = {
      println(Stream(1, 2, 3).forAll(e => e < 4))
      println(Stream(1, 2, 3).forAll(e => e < 2))
    }
  }

  def main(args: Array[String]): Unit = {
    Ex4.test()
  }

}
