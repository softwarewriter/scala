package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter5.Stream.{append, cons, empty}

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

    // TODO: Should move to trait , but then I get
    // covariant type A occurs in contravariant position in type A of value a
    // https://stackoverflow.com/questions/43180310/covariant-type-a-occurs-in-contravariant-position-in-type-a-of-value-a
    def append[A](s1: Stream[A], s2: Stream[A]): Stream[A] = {
      s1.foldRight(s2)((a, b) => cons(a, b))
    }

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
      println(Stream("ole", "dole", "doff").map(_.length).toList)
      println(Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList)
      println(append(Stream(1, 2, 3, 4), Stream(5, 6, 7, 8)).toList)
    }
  }

  def main(args: Array[String]): Unit = {
    Ex6.test()
  }

}
