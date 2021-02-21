package no.jergan.scrapbook.fpinscala

object Chapter14 {


  sealed trait ST[S, A] { self =>
    protected def run(s: S): (A, S)

    def map[B](f: A => B): ST[S, B] = new ST[S, B] {
      override protected def run(s: S): (B, S) = {
        val (a, s2) = self.run(s)
        (f(a), s2)
      }
    }

    def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
      override protected def run(s: S): (B, S) = {
        val(a, s2) = self.run(s)
        f(a).run(s2)
      }
    }
  }

  object ST {

    def apply[S, A](a: => A): ST[S, A] = {
      lazy val memo = a
      new ST[S, A] {
        override protected def run(s: S): (A, S) = (memo, s)
      }
    }

    def runST[A](st: RunnableST[A]): A =
      st.apply[String].run("pelle")._1
  }

  sealed trait STRef[S, A] {
    protected var cell: A
    def read: ST[S, A] = ST(cell)
    def write(a: A): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S): (Unit, S) = {
        cell = a
        ((), s)
      }
    }
  }

  object STRef {
    def apply[S, A](a: A): ST[S, STRef[S, A]] = ST[S, STRef[S, A]](new STRef[S, A] {
      var cell = a
    }
    )
  }

  trait RunnableST[A] {
    def apply[S]: ST[S, A]
  }

  sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
    protected def value: Array[A]

    def size: ST[S, Int] = ST(value.size)

    def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S) = {
        value(i) = a
        ((), s)
      }
    }

    def read(i: Int): ST[S, A] = ST(value(i))

    def freeze: ST[S, List[A]] = ST(value.toList)

    def fill(xs: Map[Int, A]): ST[S, Unit] = xs.keys match {
      case head :: tail => xs.get(head) match {
        case Some(a) => write(head, a).flatMap(_ => fill(xs.view.filterKeys(k => k != head).toMap))
        case None => fill(xs.view.filterKeys(k => k != head).toMap)
      }
      case nil => ST(())
    }

    def fill2(xs: Map[Int, A]): ST[S, Unit] =
      xs.foldRight[ST[S, Unit]](ST(())) { case ((k, v), st) => st.flatMap(_ => write(k, v)) }

    def swap(i: Int, j: Int): ST[S, Unit] = for {
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    } yield ()

  }

  object STArray {
    def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] = ST[S, STArray[S, A]](new STArray[S, A] {
      lazy val value = Array.fill(sz)(v)
    })

    def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] = ST[S, STArray[S, A]](new STArray[S, A] {
      lazy val value = xs.toArray
    })

  }

  def partitionI[S](arr: STArray[S, Int], i: Int, j: Int, n: Int, r: Int, pivot: Int): ST[S, Int] = {
    if (i == r) ST[S, Int](j)
    else
      arr.read(i)
        .flatMap(valI => if (valI < pivot) arr.swap(i, j).map(_ => j + 1) else ST[S, Int](j))
        .flatMap(j => partitionI(arr, i + 1, j, n, r, pivot))
  }

  def partition[S](arr: STArray[S, Int], n: Int, r: Int, pivot: Int): ST[S, Int] = {
    for {
      pivotVal <- arr.read(pivot)
      _ <- arr.swap(pivot, r)
      j <- partitionI(arr, n, n, n, r, pivotVal)
      _ <- arr.swap(j, r)
    } yield j
  }

  def qs[S](arr: STArray[S, Int], n: Int, r: Int): ST[S, Unit] = {
    if (n < r) for {
      pi <- partition[S](arr, n, r, n + (r - n) / 2)
      _ <- qs[S](arr, n, pi - 1)
      _ <- qs[S](arr, pi + 1, r)
    } yield ()
    else ST[S, Unit](())
  }

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr <- STArray.fromList(xs)
        size <- arr.size
        _ <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
    })

  def quicksort0(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    val arr = xs.toArray

    def swap(x: Int, y: Int) = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }

    def partition(n: Int, r: Int, pivot: Int) = {
      val pivotVal = arr(pivot)
      swap(pivot, r)
      var j = n
      for (i <- n until r) if (arr(i) < pivotVal) {
        swap(i, j)
        j += 1 }
      swap(j, r)
      j
    }

    def qs(n: Int, r: Int): Unit = if (n < r) {
      val pi = partition(n, r, n + (r - n) / 2)
      qs(n, pi - 1)
      qs(pi + 1, r)
    }
    qs(0, arr.length - 1)
    arr.toList
  }

  object Ex1 {
    // implemented fill
  }

  object Ex2 {
    // implemented qs and partition
  }

  def main(args: Array[String]): Unit = {
    /*
  val p: RunnableST[(Int, Int)] = new RunnableST[(Int, Int)] {
    def apply[S] = for {
      r1 <- STRef(1)
      r2 <- STRef(2)
      x  <- r1.read
      y  <- r2.read
      _  <- r1.write(y + 1)
      _  <- r2.write(x + 1)
      a  <- r1.read
      b <- r2.read
    } yield (a, b)
  }
  val r1: (Int, Int) = ST.runST(p)
  println(r1)

     */


//    println(quicksort0(List(4, 2, 1, 3))) -> 1, 2, 3, 4
//    println(quicksort(List(4, 2, 1, 3)))
    println(quicksort(List(6, 4, 1, 5, 7, 2, 3, 8)))
  }

}
