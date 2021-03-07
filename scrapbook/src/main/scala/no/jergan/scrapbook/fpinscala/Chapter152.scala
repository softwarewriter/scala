package no.jergan.scrapbook.fpinscala

import no.jergan.scrapbook.fpinscala.Chapter11.Monad
import no.jergan.scrapbook.fpinscala.Chapter152.Process.{Await, Emit, End, Halt, Kill}

object Chapter152 {

  trait Process[F[_], O] {

    def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
      case Await(req, reqv) => Await(req, reqv andThen (_.onHalt(f)))
      case Emit(h, t) => Emit(h, t.onHalt(f))
      case Halt(err) => tryy(f(err))
    }

    def ++(p: => Process[F, O]): Process[F, O] =
      this.onHalt {
        case End => p
        case thr => Halt(thr)
      }

    def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = {
      this match {
        case Halt(err) => Halt(err)
        case Emit(h, t) => tryy(f(h)) ++ t.flatMap(f)
        case Await(req, recv) => Await(req, recv andThen (_.flatMap(f)))
      }
    }

    def runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {

      // Created m to have an explicit type to bind type A of req with type A of recv. If not it does not compile
      // This is not done in solution.
      def m[A](req: F[A], recv: Either[Throwable, A] => Process[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] = {
        F.flatMap(F.attempt(req)) { e => go(tryy(recv(e)), acc) }
      }

      def go(cur: Process[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] = {
        cur match {
          case Halt(End) => F.unit(acc)
          case Halt(err) => F.fail(err)
          case Emit(h, t) => go(t, acc.appended(h))
          case Await(req, recv) => m(req, recv, acc)
        }
      }
      go(this, IndexedSeq())
    }

  }

  trait MonadCatch[F[_]] extends Monad[F] {
    def attempt[A](a: F[A]): F[Either[Throwable, A]]
    def fail[A](t: Throwable): F[A]
  }

  def await[F[_], A, O](req: F[A])(recv: Either[Throwable, A] => Process[F, O]): Process[F, O] =
    Await(req, recv)

  def tryy[F[_], O](p: => Process[F, O]): Process[F, O] = try p
  catch {
    case e: Throwable => Halt(e)
  }

  object Process {
    case class Await[F[_], A, O](req: F[A], recv: Either[Throwable, A] => Process[F, O]) extends Process[F, O]
    case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]
    case class Halt[F[_], O](err: Throwable) extends Process[F, O]

    case object End extends Exception
    case object Kill extends Exception
  }

  object Ex10 {
    // implemented runLog
  }


  def main(args: Array[String]): Unit = {
    println("pelle")
  }

}
