package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter5.Stream
import no.jergan.scrapbook.fpinscala.Chapter6.unit

import scala.::
import scala.annotation.tailrec

/**
 * First chapter done according to new version of book.
 */
object Chapter6 {


  trait RNG {
    def nextInt: (Int, RNG)
  }

  type Rand[+A] = RNG => (A, RNG)

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nextRng) = rng.nextInt
    (if (i < 0) -(i + 1) else i, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, nextRng) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), nextRng)
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  val int: Rand[Int] = _.nextInt

  case class State[S, +A](run: S => (A, S)) {

    def map[B](f: A => B): State[S, B] = {
      State(s => {
        val (a, s2) = run(s)
        (f(a), s2)
      })
    }

    def mapUsingFlatMap[B](f: A => B): State[S, B] = {
      flatMap(a => State.unit(f(a)))
    }

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
      State(s => {
        val (a, s2) = run(s)
        val (b, s3) = sb.run(s2)
        (f(a, b), s3)
      })
    }

    def map2UsingFlatMapAndMap[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
      flatMap(a => sb
          .map(b => f(a, b)))
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = {
      State(s => {
        val (a, s2) = run(s)
        f(a).run(s2)
      })
    }
  }

  object State {

    def unit[S, A](a: A) = {
      State[S, A](s => (a, s))
    }

    // TODO: Implement as foldRight as well.

    def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] = {
      def go(as: List[A], ss: List[State[S, A]], s: S): (List[A], S) = {
        ss match {
          case Nil => (as, s)
          case head :: next => head.run(s) match { case (a, s2) => go(as.appended(a), next, s2)
          }
        }
      }
      State(s => go(List.empty, ss, s))
    }
  }

  object Ex1 {
    def test(): Unit = {
      val rng = SimpleRNG(2)
      println(nonNegativeInt(rng))
    }
  }

  object Ex2 {
    def test(): Unit = {
      val rng = SimpleRNG(3)
      println(double(rng))
    }
  }

  object Ex3 {
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, rng2) = rng.nextInt
      val (d, rng3) = double(rng2)
      ((i, d), rng3)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i, d), rng2) = intDouble(rng)
      ((d, i), rng2)
    }

    def doubleDoubleDouble(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, rng2) = nonNegativeInt(rng)
      val (d2, rnd3) = nonNegativeInt(rng2)
      val (d3, rnd4) = nonNegativeInt(rnd3)
      ((d1, d2, d3), rnd4)
    }
  }

  object Ex4 {
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      if (count == 0) {
        (List.empty, rng)
      } else {
        val (i, r2) = rng.nextInt
        val (l, rn) = ints(count - 1)(r2)
        (i :: l, rn)
      }
    }

    def intsTailRecursive(count: Int)(rng: RNG): (List[Int], RNG) = {

      @tailrec
      def go(count: Int, res: List[Int], rng: RNG): (List[Int], RNG) = {
        if (count == 0) {
          (res, rng)
        } else {
          val (i, r2) = rng.nextInt
          go(count - 1, i :: res, r2)
        }
      }
      go(count, List.empty, rng)
    }

    def test(): Unit = {
      val rng = SimpleRNG(1)
      println(ints(3)(rng))
      println(intsTailRecursive(3)(rng))
    }
  }

  object Ex5 {
    def doubleUsingMap(): Rand[Double] = {
      map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
    }
  }


  object Ex6 {
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  object Ex7 {
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {

//      @tailrec: TODO: Can this be make tail recursive?
      def go(as: List[A], fs: List[Rand[A]]): Rand[List[A]] = rng => {
        fs match {
          case Nil => (as, rng)
          case ::(head, next) => {
            val (a, rng2) = head(rng)
            go(as.appended(a), next)(rng2)
          }
        }
      }
      go(List.empty, fs)
    }

    def test(): Unit = {
      def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        sequence(List.fill(count)(int))(rng)
      }

      val rng = SimpleRNG(1)
      println(ints(3)(rng))
    }
  }

  object Ex8 {

    def nonNegativeLessThan(n: Int): Rand[Int] = {
      flatMap(nonNegativeInt) { i => {
        val mod = i % n
        if (i + (n - 1) - mod >= 0) {
          unit(mod)
        } else nonNegativeLessThan(n)
      }
      }
    }

    def test() {
      val rng = SimpleRNG(1)
      println(nonNegativeLessThan(6)(rng))
    }

  }

  object Ex9 {
    def mapUsingFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
      flatMap(s)(a => unit(f(a)))
    }

    def map2UsingFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
    }

    def map2UsingFlatMapAndMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      flatMap(ra)(a => map(rb)(b => f(a, b)))
    }
  }

  object Ex10 {
    // Implemented above
  }

  object Ex11 {

    sealed trait Input
    case object Coin extends Input
    case object Turn extends Input

    case class Machine(locked: Boolean, candies: Int, coins: Int) {


    }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
      State[Machine, (Int, Int)](m => {
        val current = ((m.candies, m.coins), m)
        (inputs, m.locked, m.candies, m.coins) match {
          case (Nil, _, _, _) => current
          case (_, _, 0, _) => current
          case (::(Coin, next), false, _, _) => simulateMachine(next).run(m)
          case (::(Turn, next), true, _, _) => simulateMachine(next).run(m)
          case (::(Coin, next), true, candies, coins) => simulateMachine(next).run(Machine(false, candies, coins + 1))
          case (::(Turn, next), false, candies, coins) => simulateMachine(next).run(Machine(true, candies - 1, coins))
        }
      })
    }

    def test() = {
      val m = Machine(true, 3, 2)

      val sim: State[Machine, (Int, Int)] = simulateMachine(List(Coin, Turn, Coin, Turn))
      val ((candies, coins), finalM) = sim.run(m)
//      println(s"candies: $candies, coins: $coins")

      val v = State.unit[String, Int](42)
        .flatMap(i => State[String, Int](s => (i + 1, s + "-x")))
        .flatMap(i => State[String, Int](s => (i + 2, s + "-y")))
      println(v.run("s0"))

      val v1 = State.unit[String, Int](42)
      val v2 = v1.flatMap(i =>
        v1.flatMap(ii =>
          State[String, Int](s => (i + ii, s + "-x"))))
      println(v2.run("s0"))

      val v0 = State.unit[String, Int](42)
      val forRes = for {
        v1 <- v0
        v2 <- State[String, Int](s => (v1 + 1, s + "-x"))
        v3 <- State[String, Int](s => (v1 + v2, s + "-y"))
      } yield v3 + 2

      println(forRes.run("for"))

    }
  }

  def main(args: Array[String]): Unit = {
    Ex11.test()
  }

}
