package no.jergan.scrapbook.fpinscala

import scala.annotation.tailrec

object Chapter4 {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = {
      this match {
        case Some(value) => Some(f(value))
        case None => None
      }
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      this match {
        case Some(value) => f(value)
        case None => None
      }
    }

    def getOrElse[B >: A](default: => B): B = {
      this match {
        case Some(value) => value
        case None => default
      }
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      this match {
        case Some(value) => Some(value)
        case None => ob
      }
    }

    def filter(f: A => Boolean): Option[A] = {
      this match {
        case Some(value) => if (f(value)) Some(value) else None
        case None => None
      }
    }

  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Ex1 {

    def test() = {
      class A1
      class A2 extends A1
      class A3 extends A2

      def elseProvider: A2 = {
        println("evaluate else1")
        new A2
      }

      Some(new A2).getOrElse(elseProvider)
      None.getOrElse(elseProvider)

      def elseProvider2: Option[A2] = {
        println("evaluate else2")
        Some(new A2)
      }
      Some(new A2).orElse(elseProvider2)
      None.orElse(elseProvider2)

      println(None.orElse(None).orElse(None).orElse(Some(new A2)))

    }

  }

  def main(args: Array[String]): Unit = {
    Ex1.test()
  }

}
