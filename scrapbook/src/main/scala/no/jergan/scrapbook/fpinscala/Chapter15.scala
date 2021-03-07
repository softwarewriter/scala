package no.jergan.scrapbook.fpinscala

import java.io.{File, FileInputStream, FileOutputStream, InputStream, OutputStream, Reader, Writer}

import no.jergan.scrapbook.fpinscala.Chapter11.Monad
import no.jergan.scrapbook.fpinscala.Chapter11.Monad.optionMonad


object Chapter15 {

  sealed trait Process[I, O] {

    def apply(s: Stream[I]): Stream[O] = this match {
      case Halt() => Stream()
      case Await(recv) => s match {
        case h #:: t => recv(Some(h))(t)
        case xs => recv(None)(xs)
      }
      case Emit(h, t) => h #:: t(s)
    }

    def repeat: Process[I, O] = {
      def go(p: Process[I, O]): Process[I, O] = {
        p match {
          case Halt() => go(this)
          case Await(recv) => Await({
            case Some(i) => go(recv(Some(i)))
            case None => Halt()
          })
          case Emit(h, t) => Emit(h, go(t))
        }
      }

      go(this)
    }

    def pipe[O2](p2: Process[O, O2]): Process[I, O2] = p2 match {
      case Halt() => Halt()
      case Emit(h, t) => Emit(h, this pipe t)
      case Await(f) => this match {
        case Emit(h, t) => t pipe f(Some(h))
        case Halt() => Halt() pipe (f(None))
        case Await(g) => Await((i: Option[I]) => g(i) pipe p2)
      }
    }

    def map[O2](f: O => O2): Process[I, O2] = this pipe lift(f)

    def ++(p: => Process[I, O]): Process[I, O] = this match {
      case Halt() => p
      case Emit(h, t) => Emit(h, t ++ p)
      case Await(recv) => Await(recv andThen(a => a ++ p))
    }

    def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
      case Halt() => Halt()
      case Emit(h, t) => f(h) ++ t.flatMap(f)
      case Await(recv) => Await(recv andThen (a => a.flatMap(f)))
    }

    def withIndex: Process[I, (O, Int)] = {
      zip(this, count)
    }

  }

  case class Halt[I, O]() extends Process[I, O]
  case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
  case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]

  def monad[I]: Monad[({ type f[x] = Process[I, x]})#f] =
    new Monad[({ type f[x] = Process[I, x]})#f] {
      override def unit[A](a: => A): Process[I, A] = Emit(a)
      override def flatMap[A, B](fa: Process[I, A])(f: A => Process[I, B]): Process[I, B] = fa.flatMap(f)
    }

  def liftOne[I, O](f: I => O): Process[I, O] = {
    Await[I, O] {
      case Some(a) => Emit(f(a))
      case None => Halt()
    }
  }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def identityP[I]: Process[I, I] = lift(identity)

  def filter[I](p: I => Boolean): Process[I, I] = {
    Await[I, I]{
      case Some(i) if p(i) => Emit(i)
      case _               => Halt()
    }.repeat
  }

  def take[I](n: Int): Process[I, I] = {
    if (n == 0) Halt()
    else Await {
      case Some(i) => Emit(i, take(n - 1))
      case None => Halt()
    }
  }

  def drop[I](n: Int): Process[I, I] = {
    if (n == 0) identityP
    else Await {
      case Some(_) => drop(n - 1)
      case None => Halt()
    }
  }

  def takeWhile[I](p: I => Boolean): Process[I, I] = {
    Await {
      case Some(i) if p(i) => Emit(i, takeWhile(p))
      case _ => Halt()
    }
  }

  def dropWhile[I](p: I => Boolean): Process[I, I] = {
    Await {
      case Some(i) => if (p(i)) dropWhile(p) else Emit(i, identityP)
      case _ => identityP
    }
  }

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] = {
      Await{
        case Some(i) => Emit(acc + i, go(acc + i))
        case None => Halt()
      }
    }
    go(0)
  }

  def sumUsingLoop: Process[Double, Double] = {
    loop[Double, Double, Double](0)((i, s) => (s + i, s + i))
  }

  def count[I]: Process[I, Int] = {
    def go(n: Int): Process[I, Int] = {
      Await {
        case Some(_) => Emit(n + 1, go(n + 1))
        case _ => Halt()
      }
    }
    go(0)
  }

  def countUsingLiftAndSum[I]: Process[I, Int] = {
    lift[I, Double](_ => 1.0) pipe sum pipe lift[Double, Int](_.toInt)
  }

  def countUsingLoop[I]: Process[I, Int] = {
    loop(0)((_, s) => (s + 1, s + 1) )
  }

  def mean: Process[Double, Double] = {
    def go(n: Int, sum: Double): Process[Double, Double] = {
      Await {
        case Some(i) => {
          val sum2 = sum + i
          val n2 = n + 1
          Emit(sum2 / n2, go(n2, sum2))
        }
        case _ => Halt()
      }
    }
    go(0, 0)
  }

  def meanUsingLoop[I]: Process[Double, Double] = {
    loop((0.0, 0))((i, s) => {
      val sum2 = s._1 + i
      val count2 = s._2 + 1
      (sum2 / count2, (sum2, count2))
    })
  }

  def meanUsingZip[I]: Process[Double, Double] = {
    zip[Double, Int, Double](count, sum).map(a => a._2 / a._1)
  }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] = {
    Await {
      case Some(i) => {
        val (o, s2) = f(i, z)
        Emit(o, loop(s2)(f))
      }
      case None => Halt()
    }
  }

  def identityWithIndex[I]: Process[I, (I, Int)] = {
    loop(0)((i, s) => ((i, s + 1), s + 1))
  }

  def identityWithIndexUsingZip[I]: Process[I, (I, Int)] = {
    zip[I, I, Int](identityP, count)
  }

  def zip[I, O1, O2](p1: Process[I, O1], p2: Process[I, O2]): Process[I, (O1, O2)] = {
    // implementation only works for processes of same form.
    (p1, p2) match {
      case (Await(f1), Await(f2)) => Await(a => zip(f1(a), f2(a)))
      case (Emit(h1, t1), Emit(h2, t2)) => Emit((h1, h2), zip(t1, t2))
      case _ => Halt()
    }
  }

  def exists[I](p: I => Boolean): Process[I, Boolean] = {
    Await{
      case Some(i) => if (p(i)) Emit(true) else exists(p)
      case None => Emit(false)
    }
  }

  object Ex1 {
    // implemented take
    // implemented drop
    // implemented take while
    // implemented drop while
  }

  object Ex2 {
    // implemented count
  }

  object Ex3 {
    // implemented mean
  }

  object Ex4 {
    // implemented loop and used it for sum, count and mean
  }

  object Ex5 {
    // tried to implement pipe, failed!
  }

  object Ex6 {
    // implemented identityWithIndex
  }

  object Ex7 {
    // implemented zip
    // implemented mean using zip of count and sum
    // implemented withIndex using zip an count
  }

  object Ex8 {
    // implemented exists
  }

  object Ex9 {

    def toCelsius(fahrenheit: Double): Double = (5.0 / 9.0) * (fahrenheit - 32.0)

    def processFile[F[_]: Monad](from: File, to: File, p: Process[String, String]): F[Unit] = {
      implicitly[Monad[F]].unit {

        def go(in: Iterator[String], out: Writer, cur: Process[String, String]): Unit = {
          cur match {
            case Halt() => ()
            case Await(recv) => if (in.hasNext) go(in, out, recv(Some(in.next()))) else ()
            case Emit(h, t) => {
              out.write(h)
              go(in, out, t)
            }
          }
        }
        //      go(new FileInputStream(from), new FileOutputStream(to), p)
      }
    }

    def test() {

    processFile[Option](new File("from"), new File("to"),
      filter[String](_.trim().nonEmpty)
        pipe filter[String](l => !l.startsWith("#"))
        pipe lift(java.lang.Double.parseDouble)
        pipe lift(toCelsius)
        pipe lift[Double, String](java.lang.Double.toString))(optionMonad)
    }
  }

  def main(args: Array[String]): Unit = {

    def print[A](s: Stream[A]): Unit = {
      println(s.toList)
    }
    print(sum(Stream(1, 2, 3)))
    print(sumUsingLoop(Stream(1, 2, 3)))
    print(take(2)(Stream(1, 2, 3, 4)))
    print(drop(2)(Stream(1, 2, 3, 4)))
    print(takeWhile[Int](_ < 4)(Stream(1, 2, 3, 4)))
    print(dropWhile[Int](_ < 2)(Stream(1, 2, 3, 4)))

    print(count(Stream("a", "b", "c")))
    print(countUsingLiftAndSum(Stream("a", "b", "c")))
    print(countUsingLoop(Stream("a", "b", "c")))
    print(mean(Stream(1, 3, 5)))
    print(meanUsingLoop(Stream(1, 3, 5)))
    print(meanUsingZip(Stream(1, 3, 5)))

    print(identityWithIndex(Stream("a", "b", "c")))
    print(identityWithIndexUsingZip(Stream("a", "b", "c")))
    print(exists[String](_ == "b")(Stream("a", "b", "c")))
    print(exists[String](_ == "d")(Stream("a", "b", "c")))
  }

}
