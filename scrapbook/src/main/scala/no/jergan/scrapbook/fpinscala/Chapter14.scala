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

  object Ex1 {


  }

  def main(args: Array[String]): Unit = {

  }

}
