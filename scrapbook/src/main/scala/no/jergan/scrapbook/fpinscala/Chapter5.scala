package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter5.Ex11.ones
import no.jergan.scrapbook.fpinscala.Chapter5.Ex12.zip
import no.jergan.scrapbook.fpinscala.Chapter5.Stream.{cons, empty, unfold}

import scala.annotation.tailrec


/**
 * Last chapter done according to old version of book.
 */
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

    def drop(n: Int): Stream[A] = {
      uncons match {
        case Some((hd, tl)) => if (n == 0) this else tl.drop(n - 1)
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

    def tails: Stream[Stream[A]] = {
      unfold(this)(s => {
        s.uncons match {
          case Some((_, tl)) => Some(s, tl)
          case None => None
        }
      }).append(Stream(empty)) // TODO: Could we somehow avoid this append?
    }

    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight((empty[B], z))((a, b) => {
      val (s, z) = b
      val v: B = f(a, z)
      (cons(v, s), v)
    })._1.append(Stream[B](z)) // TODO: Could we somehow avoid this append?

    // TODO: Should redo this while redoing chapter 5 according to the new version
    //       However, I had to do this now to be able to run exercises in Chapter10
    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {

      (uncons, s2.uncons) match {
        case (Some((h1, t1)), Some((h2, t2))) => cons(f(h1, h2), t1.zipWith(t2)(f))
        case (_, _) => empty
      }
    }

    @tailrec
    final def find(p: A => Boolean): Option[A] = {
      uncons match {
        case Some((h, t)) => if (p(h)) Some(h) else t.find(p)
        case None => None
      }
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

    def from(n: Int): Stream[Int] =
      cons(n, from(n + 1))
  }

  object Ex1 {
    def test(): Unit = {
      println(Stream(1, 2, 3).toList)
    }
  }

  object Ex2 {
    def test(): Unit = {
      println(Stream(1, 2, 3).take(2).toList)
      println(Stream(1, 2, 3).drop(2).toList)
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
      unfold(1)(_ => Some(1, 1))
    }

    def constant(n: Int): Stream[Int] = {
      unfold(n)(_ => Some(n, n))
    }

    def from(n: Int): Stream[Int] = {
      unfold(n)(n => Some(n, n + 1))
    }

    def fibs(): Stream[Int] = {
      unfold((0, 1)) {case (a, b) => Some((a, (a, a + b)))}
    }

    def test() = {
      println(ones.take(3).toList)
      println(constant(42).take(3).toList)
      println(from(42).take(3).toList)
      println(fibs.take(8).toList)
    }
  }

  object Ex12 {

    def zip[A, B](s1: Stream[A], s2: Stream[B]): Stream[(A, B)] = {
      unfold((s1, s2))(s => {
        val (s1, s2) = s
        (s1.uncons, s2.uncons) match {
          case (Some((hd1, tl1)), Some((hd2, tl2))) => Some((hd1, hd2), (tl1, tl2))
          case _ => None
        }
      })
    }

    def zipAll[A, B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A], Option[B])] = {
      unfold((s1, s2))(s => {
        val (s1, s2) = s
        (s1.uncons, s2.uncons) match {
          case (Some((hd1, tl1)), Some((hd2, tl2))) => Some((Some(hd1), Some(hd2)), (tl1, tl2))
          case (Some((hd1, tl1)), None) => Some((Some(hd1), None), (tl1, empty))
          case (None, Some((hd2, tl2))) => Some((None, Some(hd2)), (empty, tl2))
          case _ => None
        }
      })
    }

    def test() = {
      println(Stream("ole", "dole", "doff").mapUsingUnfold(_.length).toList)
      println(ones.takeUsingUnfold(3).toList)
      println(Stream(1, 2, 3, 4).takeWhileUsingUnfold(_ < 3).toList)
      println(zip(Stream(1, 2, 3, 4), Stream("a", "b", "c")).toList)
      println(zipAll(Stream(1, 2, 3, 4), Stream("a", "b", "c")).toList)
    }
  }

  object Ex13 {
    def startsWith[A](s: Stream[A], pre: Stream[A]): Boolean = {
      zip(s, pre)
        .foldRight(true)((a, b) => b && (a._1 == a._2))
    }

    def test() = {
      println(startsWith(Stream(1, 2, 3, 4), Stream()))
      println(startsWith(Stream(1, 2, 3, 4), Stream(1, 2, 3, 4)))
      println(startsWith(Stream(1, 2, 3, 4), Stream(1, 2)))
      println(startsWith(Stream(1, 2, 3, 4), Stream(2, 1)))
    }
  }

  object Ex14 {

    def test() = {
      println(Stream(1, 2, 3).tails.toList.map(_.toList))
    }
  }

  object Ex15 {

    def test() = {
      // Can not be implemented using unfold, as unfold traverses the stream from head to tail
      println(Stream(1, 2, 3).scanRight(0)(_ + _).toList)
    }
  }

  def main(args: Array[String]): Unit = {
    Ex2.test()
  }

}
