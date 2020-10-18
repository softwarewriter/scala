package no.jergan.scrapbook.fpinscala

import scala.annotation.tailrec

object Chapter3 {

  sealed trait Liste[+A]

  case object Nil extends Liste[Nothing]
  case class Cons[+A](head: A, tail: Liste[A]) extends Liste[A]

  def sum(ints: Liste[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: Liste[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): Liste[A] =
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: Liste[A]): Liste[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }
  }

  def append[A](l1: Liste[A], l2: Liste[A]): Liste[A] = {
    (l1, l2) match {
      case (Nil, Nil) => Nil
      case (l1, Nil) => l1
      case (Nil, l2) => l2
      case (l1, l2) => foldRight(l1, l2)((a, b) => Cons(a, b))
    }
  }

  def foldRight[A,B](l: Liste[A], z: B)(f: (A, B) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  @tailrec
  def foldLeft[A, B](l: Liste[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def map[A, B](l: Liste[A])(f: A => B): Liste[B] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }
  }

  def flatMap[A, B](l: Liste[A])(f: A => Liste[B]): Liste[B] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => append(f(x), flatMap(xs)(f))
    }
  }

  def filter[A](l: Liste[A])(f: A => Boolean): Liste[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
    }
  }

  object Ex1 {
    def test(): Unit = {
      val x: Int = apply(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
      }
      println(x)
    }
  }

  object Ex2 {
    def test() = {
      println(tail(apply(1, 2, 3)))
    }
  }

  object Ex3 {
    @tailrec
    def drop[A](l: Liste[A], n: Int): Liste[A] = {
      l match {
        case Nil => Nil
        case Cons(x, xs) => if (n == 0) l else drop(xs, n - 1)
      }
    }

    def test() = {
      println(drop(apply(1, 2, 3), 2))
    }
  }

  object Ex4 {
    @tailrec
    def dropWhile[A](l: Liste[A], p: A => Boolean): Liste[A] = {
      l match {
        case Nil => Nil
        case Cons(x, xs) => if (p(x)) dropWhile(xs, p) else l
      }
    }

    def test() = {
      println(dropWhile(apply(1, 2, 3, 4), (a: Int) => a < 3))
    }
  }

  object Ex5 {
    def setHead[A](l: Liste[A], head: A): Liste[A] = {
      l match {
        case Nil => Nil
        case Cons(x, xs) => Cons(head, xs)
      }
    }

    def test() = {
      println(setHead(apply(1, 2, 3, 4), 42))
    }
  }

  object Ex6 {
    def init[A](l: Liste[A]): Liste[A] = {
      l match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(x, xs) => Cons(x, init(xs))
      }
    }

    def test() = {
      println(init(apply(1, 2, 3, 4)))
    }
  }

  object Ex7 {
    def productRight(l: Liste[Double]) = {
      foldRight(l, 1.0)((a, b) => a * b)
    }

    def test() = {
      println(productRight(apply(1, 2, 0, 3, 4)))
      // Can not short-circuit if using foldRight
    }
  }

  object Ex8 {
    def test() = {
      println(foldRight(apply(1, 2, 3), Nil: Liste[Int])((a, b) => Cons(a, b)))
    }
  }

  object Ex9 {
    def lengthRight[A](l: Liste[A]) = {
      foldRight(l, 0)((_, b) => b + 1)
    }

    def test() = {
      println(lengthRight(apply(0, 0, 0)))
    }
  }

  object Ex10 {
    // Implemented foldLeft
  }

  object Ex11 {
    def sumLeft(l: Liste[Int]) = {
      foldLeft(l, 0.0)(_ + _)
    }

    def productLeft(l: Liste[Double]) = {
      foldLeft(l, 1.0)((a, b) => a * b)
    }

    def lengthLeft[A](l: Liste[A]) = {
      foldLeft(l, 0)((a, _) => a + 1)
    }

    def test() = {
      println(sumLeft(apply(1, 2, 3)))
      println(productLeft(apply(1, 2, 4, 0)))
      println(lengthLeft(apply(1, 2, 3)))
    }
  }

  object Ex12 {
    def reverse[A](l: Liste[A]): Liste[A] = {
      l match {
        case Nil => l
        case Cons(x, xs) => foldLeft(xs, Cons(x, Nil))((a: Liste[A], b: A) => Cons(b, a))
      }
    }

    def test() = {
      println(reverse(apply(1, 2, 3)))
    }
  }

  object Ex13 {
    def foldLeftUsingRight[A,B](l: Liste[A], z: B)(f: (B, A) => B): B = ???

    // TODO: Not done yet.
    def test() = {
      println(foldLeft(apply("a", "b", "c"), "z")((b, a) => b + ", " + a))
      println(foldLeftUsingRight(apply("a", "b", "c"), "z")((b, a) => b + ", " + a))

      // The other way is not possible, as foldLeft is tail recursive and foldRight is not.
      // If it was possible, it would be a curious performance improvement.
    }
  }

  object Ex14 {
    def test() = {
      println(append(apply(1, 2, 3), apply(4, 5, 6)))
    }
  }

  object Ex15 {
    def flatten[A](l: Liste[Liste[A]]): Liste[A] = {
      (l) match {
        case Nil => Nil
        case _ => foldRight(l, Nil: Liste[A])((a, b) => append(a, b))
      }
    }

    def test() = {
      println(flatten(apply(apply(1, 2, 3), apply(4, 5, 6), apply(7, 8, 9))))
    }
  }

  object Ex16 {
    def addOne(l: Liste[Int]): Liste[Int] = {
      l match {
        case Nil => Nil
        case Cons(x, xs) => Cons(x + 1, addOne(xs))
      }
    }
  }

  object Ex17 {
    def doubleToStrng(l: Liste[Double]): Liste[String] = {
      l match {
        case Nil => Nil
        case Cons(x, xs) => Cons(x.toString, doubleToStrng(xs))
      }
    }

    def test() = {
      println(doubleToStrng(apply(1.1, 2.2, 3.3)))
    }
  }

  object Ex18 {
    def test() = {
      println(map(apply(1, 2, 3))(a => a + 1))
    }
  }

  object Ex19 {
    def test() = {
      println(filter(apply(1, 2, 3, 4, 5))(_ % 2 == 0))
    }
  }

  object Ex20 {
    def test() = {
      println(flatMap(apply(1, 2, 3))(i => apply(i,i)))
    }
  }

  object Ex21 {
    def filterUsingFlatMap[A](l: Liste[A])(f: A => Boolean): Liste[A] = {
      l match {
        case Nil => Nil
        case _ => flatMap(l)((a: A) => if (f(a)) apply(a) else Nil)
      }
    }

    def test() = {
      println(filterUsingFlatMap(apply(1, 2, 3, 4, 5))(_ % 2 == 0))
    }
  }

  object Ex22AndEx23 {

    def zip[A](l1: Liste[A], l2: Liste[A])(add: (A, A) => A): Liste[A] = {
      (l1, l2) match {
        case (Nil, Nil) => Nil
        case (l1, Nil) => l1
        case (Nil, l2) => l2
        case (Cons(x, xs), Cons(y, ys)) => Cons(add(x, y), zip(xs, ys)(add))
      }
    }

    def test() = {
      println(zip(apply(1, 2, 3, 0), apply(4, 5, 6))((a, b) => a + b))
    }
  }

  object Ex24 {
    @tailrec
    def hasSubsequence[A](l: Liste[A], sub: Liste[A]): Boolean = {

      @tailrec
      def hasAtStart(l: Liste[A], sub: Liste[A]): Boolean = {
        (l, sub) match {
          case (_, Nil) => true
          case (Nil, _) => false
          case (Cons(x, xs), Cons(y, ys)) => if (x == y) hasAtStart(xs, ys) else false
        }
      }

      (l, sub) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(x, xs), sub) => if (hasAtStart(l, sub)) true else hasSubsequence(xs, sub)
      }
    }

    def test() = {
      val l = apply(1, 2, 3, 4, 5)
      println(hasSubsequence(Nil, Nil))
      println(!hasSubsequence(Nil, apply(1)))

      println(hasSubsequence(l, Nil))
      println(hasSubsequence(l, apply(1)))
      println(hasSubsequence(l, apply(1, 2)))
      println(hasSubsequence(l, apply(4, 5)))
      println(hasSubsequence(l, apply(5)))
      println(!hasSubsequence(l, apply(1, 3, 5)))

      println(hasSubsequence(apply(1, 2, 1, 2, 3), apply(1, 2, 3)))
      println(hasSubsequence(apply(1, 1, 1, 2), apply(1, 1, 2)))
    }
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size(tree: Tree[_]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => size(left) + size(right)
    }
  }

  def max(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(value) => value
      case Branch(left, right) => max(left).max(max(right))
    }
  }

  def depth(tree: Tree[Any]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + depth(left).max(depth(right))
    }
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  def fold[A, B](tree: Tree[A])(fLeaf: A => B)(fBranch: (B, B) => B): B = {
    tree match {
      case Leaf(value) => fLeaf(value)
      case Branch(left, right) => fBranch(fold(left)(fLeaf)(fBranch), fold(right)(fLeaf)(fBranch))
    }
  }

  def foldWithZ[A, B](tree: Tree[A], z: B)(fLeaf: (B, A) => B)(fBranch: (B, B) => B): B = {
    tree match {
      case Leaf(value) => fLeaf(z, value)
      case Branch(left, right) => fBranch(foldWithZ(left, z)(fLeaf)(fBranch), foldWithZ(right, z)(fLeaf)(fBranch))
    }
  }

  val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(55))))

  object Ex25 {
    def test() = {
      println(size(tree))
    }
  }

  object Ex26 {
    def test() = {
      println(max(tree))
    }
  }

  object Ex27 {
    def test() = {
      println(depth(tree))
    }
  }

  object Ex28 {
    def test() = {
      println(map(tree)(_ + 1))
    }
  }

  object Ex29 {
    def sizeByFold(tree: Tree[_]): Int = {
      foldWithZ(tree, 0)((b, _) => b + 1)((left, right) => left + right)
    }

    def maxByFold(tree: Tree[Int]): Int = {
      fold(tree)(a => a)((left, right) => left.max(right))
    }

    def depthByFold(tree: Tree[Any]): Int = {
      foldWithZ(tree, 0)((b, _) => b + 1)((left, right) => 1 + left.max(right))
    }

    def mapByFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
      fold[A, Tree[B]](tree)(a => Leaf(f(a))) ((left, right) => Branch(left, right))
    }

    def test() = {
      println(sizeByFold(tree))
      println(maxByFold(tree))
      println(depthByFold(tree))
      println(mapByFold(tree)(_ + 1))
    }
  }

  def main(args: Array[String]): Unit = {
    Ex29.test()
  }

}
