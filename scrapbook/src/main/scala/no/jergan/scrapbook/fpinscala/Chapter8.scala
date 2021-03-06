package no.jergan.scrapbook.fpinscala


import no.jergan.scrapbook.fpinscala.Chapter5.Stream
import no.jergan.scrapbook.fpinscala.Chapter6.{RNG, SimpleRNG, State, map, nonNegativeInt}
import no.jergan.scrapbook.fpinscala.Chapter7.Par
import no.jergan.scrapbook.fpinscala.Chapter8.Prop.{FailedCase, Falsified, MaxSize, Passed, Proved, Result, SuccessCount, TestCases, forAll, run}
import no.jergan.scrapbook.fpinscala.Chapter8.Gen._
import no.jergan.scrapbook.fpinscala.Chapter8.SGen.{listOf, listOf1}

/**
 * Have done all exercises to (and including 14), skipped 15, 16, 17, 19 and 20.
 */
object Chapter8 {

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

    def &&(p: Prop): Prop = {
      Prop { (max, n, rng) =>
        run(max, n, rng) match {
          case Passed | Proved => p.run(max, n, rng)
          case x => x
        }
      }
    }

    def ||(p: Prop): Prop = {
      Prop { (max, n, rng) =>
        run(max, n, rng) match {
          case Passed => Passed
          case Proved => Proved
          case Falsified(f, _) => p.tag(f).run(max, n, rng)
        }
      }
    }

    def tag(message: String): Prop = {
      Prop { (max, n, rng) =>
        run(max, n, rng) match {
          case Passed => Passed
          case Proved => Proved
          case Falsified(f, s) => Falsified(f + s"\n + $message", s)
        }
      }
    }

  }

  object Prop {
    type MaxSize = Int
    type TestCases = Int
    type FailedCase = String
    type SuccessCount = Int

    sealed trait Result {
      def isFalsified: Boolean
    }

    case object Passed extends Result {
      def isFalsified = false
    }

    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
      def isFalsified = true
    }

    case object Proved extends Result {
      def isFalsified = false
    }

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop = {
      // TODO: Was forAll(g(_))(f)
      forAll(g.forSize)(f)
    }

    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = {
      Prop {
        (max, n, rng) =>
          val casesPerSize = (n - 1) / max + 1
          val props: Stream[Prop] =
            Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
          val prop: Prop =
            props.map(p => Prop { (max, n, rng) =>
              p.run(max, casesPerSize, rng)
            }).toList.reduce(_ && _)
          prop.run(max, n, rng)
      }
    }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      randomStream(as)(rng).zipWith(Stream.from(0))((a, b) => (a, b)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
      Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
    }

    def buildMsg[A](s: A, e: Exception): String = {
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
    }

    def check(p: => Boolean): Prop = Prop { (_, _, _) => if (p) Passed else Falsified("()", 0)
    }

    def run(p: Prop,
            maxSize: Int = 100,
            testCases: Int = 100,
            rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit = p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

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

    def unsized: SGen[A] = {
      SGen(_ => this)
    }

  }

  object Gen {

    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      Gen(State(map(nonNegativeInt)(n => start + n % (stopExclusive - start))))
    }

    def int: Gen[Int] = {
      Gen(State(map(Chapter6.nonNegativeInt)(identity)))
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

  // Todo: SGen[A] was SGen[+A]
  case class SGen[A](forSize: Int => Gen[A]) {

    def map[B](f: A => B): SGen[B] = {
      SGen(n => forSize(n).map(f))
    }

    def flatMap[B](f: A => Gen[B]): SGen[B] = {
      SGen(n => forSize(n).flatMap(f))
    }

    def listOfN(size: Int): SGen[List[A]] = {
      SGen(n => forSize(n).listOfN(size))
    }

    def listOfN(size: Gen[Int]): SGen[List[A]] = {
      SGen(n => forSize(n).listOfN(size))
    }

  }

  object SGen {
    def listOf[A](g: Gen[A]): SGen[List[A]] = {
      SGen(n => g.listOfN(n))
    }

    def listOf1[A](g: Gen[A]): SGen[List[A]] = {
      SGen(n => g.listOfN(n max 1))
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

  object Ex9 {
    // Implemented && and ||
  }

  object Ex10 {
    // Implemented unsized
  }

  object Ex11 {
    // Implemented functions on SGen
  }

  object Ex12 {
    // Implemented listOf
  }

  object Ex13 {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
  }

  object Ex14 {
    val smallInt = Gen.choose(-10, 10)
    val sortedProp = forAll(listOf1(smallInt)) { ns =>
      ns.zipAll(ns.drop(1), Int.MinValue, Int.MaxValue)
        .map(pair => pair._1 <= pair._2)
        .reduce(_ && _)
    }
  }

  object Ex15 {

    def checkAll[A](as: List[A], p: A => Boolean): Prop = Prop {
      (_, _, _) => if (as.map(a => p(a)).reduce(_ && _)) Passed else Falsified("()", 0)
    }

  }

  object Ex16 {

    // Skipped for now
  }

  object Ex17 {

    // Skipped for now
  }

  object Ex18 {
    // l.takeWhile + l.dropWhile = l.length
  }

  object Ex19 {
    def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
      g.map {i => s => s.length + i}
  }

  def main(args: Array[String]): Unit = {
    Ex5.test()
  }


}
