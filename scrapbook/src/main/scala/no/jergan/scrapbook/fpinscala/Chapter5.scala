package no.jergan.scrapbook.fpinscala

import java.util

import no.jergan.scrapbook.fpinscala.Chapter3.Liste
import no.jergan.scrapbook.fpinscala.Chapter5.Ex11.ones
import no.jergan.scrapbook.fpinscala.Chapter5.Stream.{cons, empty, unfold, zip}

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

    def takeUsingUnfold(n: Int): Stream[A] = {
      unfold((this, n)) (s => {
        val (stream, n) = s
        if (n == 0) None else stream.uncons match {
          case Some((hd, tl)) => Some((hd, (tl, n - 1)))
          case None => None
        }
      })
    }

    def takeWhile(p: A => Boolean): Stream[A] = {
      uncons match {
        case Some((hd, tl)) => if (p(hd)) cons(hd, tl.takeWhile(p)) else empty
        case None => empty
      }
    }

    def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] = {
      foldRight[Stream[A]](empty)((a, b) => if (p(a)) cons(a, b) else empty)
    }

    def takeWhileUsingUnfold(p: A => Boolean): Stream[A] = {
      unfold(this)(_.uncons match {
        case Some((hd, tl)) => if (p(hd)) Some((hd, tl)) else None
        case None => None
      })
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

    def map[B](f: A => B): Stream[B] = {
      foldRight(empty[B])((a, b) => cons(f(a), b))
    }

    def mapUsingUnfold[B](f: A => B): Stream[B] = {
      unfold(this)(_.uncons match {
        case Some((hd, tl)) => Some((f(hd), tl))
        case None => None
      })
    }

    def filter(p: A => Boolean): Stream[A] = {
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)
    }

    def append[B >: A](s: Stream[B]): Stream[B] = {
      this.foldRight(s)((a, b) => cons(a, b))
    }

    def flatMap[B >: A](f: B => Stream[B]): Stream[B] = {
      foldRight(empty[B])((a, b) => f(a).append(b))
    }


  }

  object Stream {
    def empty[A]: Stream[A] =
      new Stream[A] {
        def uncons = None
      }

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      new Stream[A] {
        lazy val uncons = Some((hd, tl))
      }
    }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case Some((a, s)) => cons(a, unfold(s)(f))
        case None => empty[A]
      }
    }

    def zip[A](s1: Stream[A], s2: Stream[A], add: (A, A) => A): Stream[A] = {
      unfold((s1, s2))(s => {
        val (s1, s2) = s
        (s1.uncons, s2.uncons) match {
          case (Some((hd1, tl1)), Some((hd2, tl2))) => Some(add(hd1, hd2), (tl1, tl2))
          case _ => None
        }
      })
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

      println(Stream(1, 2, 3, 4).flatMap(a => Stream(a, a)).toList)
    }
  }

  object Ex7 {
    def constant[A](a: A): Stream[A] = {
      cons(a, constant(a))
    }

    def test() = {
      println(constant(42).take(3).toList)
    }
  }

  object Ex8 {
    def from(n: Int): Stream[Int] = {
      cons(n, from(n + 1))
    }

    def test() = {
      println(from(42).take(3).toList)
    }
  }

  object Ex9 {
    def fibs(a: Int, b: Int): Stream[Int] = {
      cons(a, fibs(a + b, a))
    }

    def test() = {
      println(fibs(0, 1).take(8).toList)
    }
  }

  object Ex10 {
    // unfold put in Stream companion object
  }

  object Ex11 {

    def ones: Stream[Int] = {
      unfold(1)(_ => Option(1, 1))
    }

    def constant(n: Int): Stream[Int] = {
      unfold(n)(_ => Option(n, n))
    }

    def from(n: Int): Stream[Int] = {
      unfold(n)(n => Option(n, n + 1))
    }

    def fibs(): Stream[Int] = {
      unfold((0, 1)) (a => Option(a._1, (a._1 + a._2, a._1)))
    }

    def test() = {
      println(ones.take(3).toList)
      println(constant(42).take(3).toList)
      println(from(42).take(3).toList)
      println(fibs.take(8).toList)
    }
  }

  object Ex12 {

    def test() = {
      println(Stream("ole", "dole", "doff").mapUsingUnfold(_.length).toList)
      println(ones.takeUsingUnfold(3).toList)
      println(Stream(1, 2, 3, 4).takeWhileUsingUnfold(_ < 3).toList)
      println(zip[Int](Stream(1, 2, 3, 4), Stream(4, 5, 6), (a, b) => a + b).toList)
    }
  }

  def main(args: Array[String]): Unit = {
    Ex12.test()
  }

}
