package no.jergan.scrapbook.scalawithcats

import cats.{Applicative, Foldable, Monoid}
import cats.implicits.{toFoldableOps, toTraverseOps}

object Ex7221 {

  def traverse[G[_]: Applicative, A, B](l: Seq[A], f: A => G[B]): G[Seq[B]] =
    l.foldLeft(Applicative[G].pure(Seq[B]()))((b, a) => Applicative[G].map2(f(a), b)(_ +: _))

  def main(args: Array[String]): Unit = {
    val v1   = Vector(1, 2)
    val v2   = Vector(3, 4)
    val list = List(v1, v2)
    val vector = list.sequence
    println(vector)
  }

}
