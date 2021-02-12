package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter11.Monad
import no.jergan.scrapbook.fpinscala.Chapter7.Par

import scala.io.StdIn.readLine


object Chapter13 {

  sealed trait Free[F[_], A] {

    def map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
  }

  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  def freeMonad[F[_]]: Monad[({type f[x] = Free[F, x]})#f] = new Monad[({type f[x] = Free[F, x]})#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = FlatMap(fa, f)
  }

  @annotation.tailrec
  def runTrampoline[A](freeA: Free[Function0, A]): A = {
    freeA match {
      case Return(a) => a
      case Suspend(s) => s.apply()
      case FlatMap(s, f) => s match {
        case Return(a) => runTrampoline(f(a))
        case Suspend(s) => runTrampoline(f(s.apply()))
        case FlatMap(ss, g) => runTrampoline(ss.flatMap(a => g(a)).flatMap(f))
      }
    }
  }

  @annotation.tailrec
  def step[F[_], A](freeA: Free[F, A]): Free[F, A] = freeA match {
    case FlatMap(FlatMap(x, f), g) => step(x.flatMap(a => f(a).flatMap(g)))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => freeA
  }

  def run[F[_], A](freeA: Free[F, A])(implicit F: Monad[F]): F[A] = step(freeA) match {
    case Return(a) => F.unit(a)
    case Suspend(s) => s
    case FlatMap(Suspend(ss), f) => F.flatMap(ss)(a => run(f(a)))
    case _ => sys.error("Should not happen")
  }

  sealed trait Console[A] {
    def toPar: Par[A]
    def toThunk: () => A
  }

  case object ReadLine extends Console[Option[String]] {
    def toPar: Par[Option[String]] = Par.lazyUnit(run)
    def toThunk: () => Option[String] = () => run

    def run: Option[String] =
      try Some(readLine())
      catch { case e: Exception => None }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    def toPar: Par[Unit] = Par.lazyUnit(println(line))
    def toThunk: () => Unit = () => println(line)
  }

  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~> [F[_], G[_]] = Translate[F, G]

  object Console {
    type ConsoleIO[A] = Free[Console, A]
    def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
    def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
  }

  val consoleToFunction0: Console ~> Function0 = new (Console ~> Function0) {
    def apply[A](a: Console[A]): () => A = a.toThunk
  }

  val consoleToPar: Console ~> Par = new (Console ~> Par) {
    def apply[A](a: Console[A]): Par[A] = a.toPar
  }

  def runFree[F[_],G[_],A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] = step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r),f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Should not happen")
    }

  implicit val function0Monad: Monad[Function0] = new Monad[Function0] {
    def unit[A](a: => A): () => A = () => a
    def flatMap[A, B](a: Function0[A])(f: A => Function0[B]): Function0[B] = () => f(a())()
  }

  implicit val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = Par.fork(Par.flatMap(a)(f))
  }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A = runFree[Console, Function0, A](a)(consoleToFunction0)(function0Monad)
  def runConsolePar[A](a: Free[Console, A]): Par[A] = runFree[Console, Par, A](a)(consoleToPar)(parMonad)

  def translate[F[_], G[_], A](freeFA: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[AA] = Free[G, AA]
    val fFreeG: Translate[F, FreeG] = new Translate[F, FreeG] {
      override def apply[A](f: F[A]): FreeG[A] = Suspend(fg(f))
    }
    val monadFreeG: Monad[FreeG] = freeMonad[G]
    runFree(freeFA)(fFreeG)(monadFreeG)
  }

  def runConsole[A](freeConsoleA: Free[Console, A]): A = {
    val t: Translate[Console, Function0] = new Translate[Console, Function0] {
      override def apply[A](f: Console[A]): () => A = f.toThunk
    }
    val freeFunction0A: Free[Function0, A] = translate[Console, Function0, A](freeConsoleA)(t)
    runTrampoline(freeFunction0A)
  }

  object Ex1 {
    // implemented map and flatMap in trait Free
    // implemented def freeMonad
  }

  object Ex2 {
    // implemented runTrampoline. IntelliJ struggles with the types but solution is correct and compiles with sbt.
  }

  object Ex3 {
    // implemented run
  }

  object Ex4 {
    // implemented translate and
  }

  def main(args: Array[String]): Unit = {

  }


}
