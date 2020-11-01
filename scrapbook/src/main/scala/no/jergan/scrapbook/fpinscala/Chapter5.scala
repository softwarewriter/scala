package no.jergan.scrapbook.fpinscala

import java.util

import no.jergan.scrapbook.fpinscala.Chapter5.Stream.{cons, empty}

object Chapter5 {

  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]

    def isEmpty: Boolean = uncons.isEmpty

    def toList: List[A] = {
      uncons match {
        case Some((hd, tl)) => List(hd) ++ tl.toList
        case None => List.empty
      }
    }

    def take(n: Int): Stream[A] = {
      uncons match {
        case Some((hd, tl)) => if (n == 0) empty else cons(hd, tl.take(n - 1))
        case None => empty
      }
    }

    def takeWhile(p: A => Boolean): Stream[A] = {
      uncons match {
        case Some((hd, tl)) => if (p(hd)) cons(hd, tl.takeWhile(p)) else empty
        case None => empty
      }
    }

    def forAll(p: A => Boolean): Boolean = {
      uncons match {
        case Some((hd, tl)) => if (p(hd)) tl.forAll(p) else false
        case None => true
      }
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      uncons match {
        case Some((h, t)) => f(h, t.foldRight(z)(f))
        case None => z
      }

    def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] = {
      foldRight[Stream[A]](empty)((a, b) => if (p(a)) cons(a, b) else empty)
    }

    def map[B](f: A => B): Stream[B] = {
      foldRight(empty[B])((a, b) => cons(f(a), b))
    }

    def filter(p: A => Boolean): Stream[A] = {
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)
    }

    // A extends B
    def append[B >: A](s: Stream[B]): Stream[B] = {

      val thisA: Stream[A] = this
      val thisB: Stream[B] = this
//      val sA: Stream[A] = s
      val sB: Stream[B] = s

      //      val v2: Stream[A] = s
      this.foldRight(s)((a, b) => cons(a, b))
    }

    /*
    def append[A](s1: Stream[A], s2: Stream[A]): Stream[A] = {
      s1.foldRight(s2)((a, b) => cons(a, b))
    }

     */
//    def append(b: b): Unit = ???

//    def append(s: Stream[A]): Unit = ???

//    def flatMap[B](f: A => Stream[A]): Stream[A] = ???
//      foldRight(empty[B])((a, b) => cons(f(a), b))

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

  object Ex5 {
    def test(): Unit = {
      println(Stream(1, 2, 3, 4, 5, 6, 7, 8).takeWhileUsingFoldRight(e => e < 4).toList)
      println(Stream(1, 2, 3, 2, 1).takeWhileUsingFoldRight(e => e < 4).toList)
    }
  }

  object Ex6 {

    def test(): Unit = {

      class A1(a1: Int) {

        override def toString() = "A1 with " + a1
      }

      class A2(a2: Int) extends A1(42) {

        override def toString() = "A2 with " + a2
      }

      println(Stream("ole", "dole", "doff").map(_.length).toList)
      println(Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList)
      println(Stream(1, 2, 3, 4).append(Stream(5, 6, 7, 8)).toList)

      val s1: Stream[A1] = Stream[A1](new A1(1), new A1(2))
      val s2: Stream[A2] = Stream[A2](new A2(3), new A2(4))
      val s21: Stream[A1] = s2

      val s3: Stream[A1] = s1.append[A1](s21)
      val s4: Stream[A1] = s2.append[A1](s1)

      println(s3.toList)
      println(s4.toList)

      //      println(Stream(1, 2, 3, 4).flatMap(a => Stream(a, a)))
    }
  }

  def main(args: Array[String]): Unit = {
    Ex6.test()
  }

}
