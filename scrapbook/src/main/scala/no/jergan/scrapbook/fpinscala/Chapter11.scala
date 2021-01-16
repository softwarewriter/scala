package no.jergan.scrapbook.fpinscala

object Chapter11 {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def andThen[A, B, C](fa: F[A])(f: A => B)(g: B => C): F[C] = {
      map(map(fa)(f))(g)
    }

    def mapPair[A, B, C](fab: F[(A, B)])(f: (A, B) => C): F[C] = {
      map(fab)(ab => f(ab._1, ab._2))
    }

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = {
      (map(fab)(_._1), map(fab)(_._2))
    }

    def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = {
      e match {
        case Left(fa) => map(fa)(Left(_))
        case Right(fb) => map(fb)(Right(_))
      }

      /*
         requires flatMap
      def join[A, B](fa: F[A], fb: F[B]): F[(A, B)]
       */

      /*
         requires flatMap
      def map2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] = {
        map(fa)(a => map(fb)(b => f(a, b)))
      }
       */
    }

}

  def main(args: Array[String]): Unit = {

  }

}
