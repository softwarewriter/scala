package no.jergan.scrapbook.fpinscala

import scala.collection.mutable

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

  sealed abstract class STMap[S, K, V] {
    protected def value: mutable.HashMap[K, V]

    def toMap: ST[S, Map[K, V]] = ST.apply(value.toMap)

    def get(k: K): ST[S, Option[V]] = new ST[S, Option[V]] {
      override protected def run(s: S): (Option[V], S) =
        (value.get(k), s)
    }

    def put(k: K, v: V): ST[S, Unit] = new ST[S, Unit] {
      override protected def run(s: S): (Unit, S) = {
        value.put(k, v)
        ((), s)
      }
    }

    def size(): ST[S, Int] = new ST[S, Int] {
      override protected def run(s: S): (Int, S) = {
        (value.size, s)
      }
    }

  }

  object STMap {

    def fromMap[S, K, V](m: Map[K, V]): ST[S, STMap[S, K, V]] = ST[S, STMap[S, K, V]](new STMap[S, K, V] {
      lazy val value: mutable.HashMap[K, V] = collection.mutable.HashMap(m.toSeq: _*)
    })

  }

  def mapProgram(map: Map[String, Int]): Map[String, Int] = ST.runST(new RunnableST[Map[String, Int]] {
      def apply[S]: ST[S, Map[String, Int]] = for {
        m <- STMap.fromMap(map)
        _ <- m.put("dole", 43)
        oleV <- m.get("ole")
        _ <- m.put("doffen", oleV.getOrElse(0) * 2)
        size <- m.size()
        _ <- m.put("size", size)
        result <- m.toMap
      } yield result
    })

/*

    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr <- STArray.fromList(xs)
        size <- arr.size
        _ <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
    })
  }
 */

  object Ex1 {
    // implemented fill
  }

  object Ex2 {
    // implemented qs and partition
  }

  object Ex3 {
    // Created trait for mutable map
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
//    println(quicksort(List(6, 4, 1, 5, 7, 2, 3, 8)))

    println(mapProgram(Map("ole" -> 42)))
  }

}
