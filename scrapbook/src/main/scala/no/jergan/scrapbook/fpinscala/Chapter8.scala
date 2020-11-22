package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter6.{RNG, State, nonNegativeInt, map}
import no.jergan.scrapbook.fpinscala.Chapter8.Prop.{FailedCase, SuccessCount}


object Chapter8 {

  trait Prop {
    def check: Either[(FailedCase, SuccessCount), SuccessCount]

    def &&(p: Prop): Prop = {
      new Prop {
        def check = {
          (this.check, p.check) match {
            case (Right(sc1), Right(sc2)) => Right(sc1 + sc2)

          }
        }
      }
    }
  }

  object Prop {
    type FailedCase = String
    type SuccessCount = Int
  }

  case class Gen[A](sample: State[RNG, A]) {
    def map[B](f: A => B): Gen[B] = {
      Gen(sample.map(f))
    }
  }

//   case class State[S, +A](run: S => (A, S)) {

  object Gen {

    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      Gen(State(map(nonNegativeInt)(n => start + n % (stopExclusive - start))))
    }

    def unit[A](a: => A): Gen[A] = {
      Gen(State(rng => (a, rng)))
    }

    def boolean: Gen[Boolean] = {
      choose(0, 2).map(_ == 0)
    }

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = ???
  }

  /*
  case class And(p1: Prop, p2: Prop) extends Prop {
    def check: Either[(FailedCase, SuccessCount), SuccessCount] =
      (p1, p2) match {
        case (Right(sc1), Right(sc2)) => Right(sc1 + sc2)
      }

    def check: Boolean = p1.check && p2.check
  }

   */

  def Ex1(): Unit = {
    // Find some properties
  }

  def Ex2(): Unit = {
    // Find some properties
  }

  def Ex3(): Unit = {
    // implemented &&
  }

  def Ex4(): Unit = {
    // implemented choose
  }

  def Ex5(): Unit = {
    // implemented unit, boolean and listofn
  }

  def main(args: Array[String]): Unit = {
    println("pelle")



  }


}
