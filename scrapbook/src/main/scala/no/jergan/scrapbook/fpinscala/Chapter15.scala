package no.jergan.scrapbook.fpinscala


object Chapter15 {
  /*
    sealed trait Stream[+A]

    case object Empty extends Stream[Nothing]

    case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

    object Stream {
      def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
      }

      def empty[A]: Stream[A] = Empty

      def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }

    object #:: {
      def unapply[A](s: LazyList[A]): Option[(A, LazyList[A])] =
        if (s.nonEmpty) Some((s.head, s.tail)) else None
      @deprecated("Prefer LazyList instead", since = "2.13.0")
      def unapply[A](s: Stream[A]): Option[(A, Stream[A])] =
        if (s.nonEmpty) Some((s.head, s.tail)) else None
    }

   */

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

  }

  case class Halt[I, O]() extends Process[I, O]
  case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
  case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]


  def liftOne[I, O](f: I => O): Process[I, O] = {
    Await[I, O] {
      case Some(a) => Emit(f(a))
      case None => Halt()
    }
  }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] = {
    Await[I, I]{
      case Some(i) if p(i) => Emit(i)
      case _               => Halt()
    }.repeat
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

  def take[I](n: Int): Process[I, I] = {
    if (n == 0) lift[I, I](identity)
    else Await {
      case Some(_) => take(n - 1)
      case None => Halt()
    }
  }

  object Ex1 {
    // implemented take
    // implemented
  }

  def main(args: Array[String]): Unit = {

    def print[A](s: Stream[A]): Unit = {
      println(s.toList)
    }

    val in: Stream[String] = Stream("a", "b", "c")

    //    print(Emit("d")(in))
    //    print(Halt()(in))

    def nop[A]: Process[A, A] = liftOne[A, A](a => a)

    //    print(nop[String](in))
    print(filter[String](e => e != "b")(in))


    print(sum(Stream(1, 2, 3)))
    print(take(2)(Stream(1, 2, 3, 4)))

  }

}
