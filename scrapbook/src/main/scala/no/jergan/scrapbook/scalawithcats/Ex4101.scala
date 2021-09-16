package no.jergan.scrapbook.scalawithcats

import cats.Monad
import cats.implicits.{toFlatMapOps, toFunctorOps}


object Ex4101 {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {

    override def pure[A](x: A): Tree[A] = leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      case Leaf(a) => f(a)
    }

    override def tailRecM[A, B](x: A)(f: A => Tree[Either[A, B]]): Tree[B] = flatMap(f(x)) {
      case Left(a) => tailRecM(a)(f)
      case Right(b) => pure(b)
    }

  }

  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = branch(branch(leaf(1), leaf(2)), leaf(3))
    val tree2: Tree[Int] = branch(branch(leaf(1), leaf(2)), leaf(3))
    println(tree)
    println(Monad[Tree].map(tree)(_ * 2))
    println(Monad[Tree].flatMap(tree)(v => branch(leaf(v), leaf(v))))

    val r = for {
      v <- tree
      d2 <- branch(leaf(v), leaf(v))
    } yield d2
    println(r)
  }

}
