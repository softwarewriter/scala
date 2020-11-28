package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter6.{RNG, SimpleRNG, State, map, nonNegativeInt}
import no.jergan.scrapbook.fpinscala.Chapter8.Prop.{FailedCase, SuccessCount}
import no.jergan.scrapbook.fpinscala.Chapter8.Gen._


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

  /*
  case class And(p1: Prop, p2: Prop) extends Prop {
    def check: Either[(FailedCase, SuccessCount), SuccessCount] =
      (p1, p2) match {
        case (Right(sc1), Right(sc2)) => Right(sc1 + sc2)
      }

    def check: Boolean = p1.check && p2.check
  }

   */

  object Prop {
    type FailedCase = String
    type SuccessCount = Int
  }

  case class Gen[A](sample: State[RNG, A]) {
    def map[B](f: A => B): Gen[B] = {
      Gen(sample.map(f))
    }

    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      Gen(sample.flatMap(a => f(a).sample))
    }

    def listOfN(size: Int): Gen[List[A]] = {
      Gen.listOfN(size, this)
    }

    def listOfN(size: Gen[Int]): Gen[List[A]] = {
      size.flatMap(n => listOfN(n))
    }
  }

  object Gen {

    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      Gen(State(map(nonNegativeInt)(n => start + n % (stopExclusive - start))))
    }

    def double(): Gen[Double] = {
      Gen(State(map(Chapter6.double)(identity)))
    }

    def unit[A](a: => A): Gen[A] = {
      Gen(State.unit(a))
    }

    def boolean: Gen[Boolean] = {
      choose(0, 2).map(_ == 0)
    }

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      Gen(State.sequence(List.fill(n)(g.sample)))
    }

    def genOption[A](ga: Gen[A]): Gen[Option[A]] = {
      ga.map(a => Some(a))
    }

    def genPair[A](ga: Gen[A]): Gen[(A, A)] = {
      listOfN(2, ga)
        .map(l => (l(0), l(1)))
    }

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
      boolean.flatMap(b => if (b) g1 else g2)
    }

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      double.flatMap(d => if (d * (g1._2 + g2._2) < g1._2) g1._1 else g2._1)
    }

  }

  object Ex1 {
    // Find some properties
  }

  object Ex2 {
    // Find some properties
  }

  object Ex3 {
    // implemented &&
  }

  object Ex4 {
    // implemented choose
  }

  object Ex5 {
    // implemented unit, boolean and listofn

    def test() = {
      val rng = new SimpleRNG(0)
      val g: Gen[Int] = choose(0, 2)
      val gList: Gen[List[Int]] = listOfN(4, g)

      println(gList.sample.run(rng)._1)
    }
  }

  object Ex6 {
    // implemented flatMap and new listOfN

    def test(): Unit = {
      val rng = new SimpleRNG(2)
      val gb = boolean
      val gi: Gen[Int] = choose(2, 4)

      val gList = gb.listOfN(gi)
      println(gList.sample.run(rng)._1)

    }
  }

  object Ex7 {
    // Implemented union
  }

  object Ex8 {
    // Implemented weighted
  }

  def main(args: Array[String]): Unit = {
    Ex6.test()


  }


}
