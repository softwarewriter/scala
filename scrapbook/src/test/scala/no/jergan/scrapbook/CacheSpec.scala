package no.jergan.scrapbook

import cats.{Applicative, Monad}
import cats.effect.{IO, Sync, Timer}
import no.jergan.scrapbook.RefCache.{Cache, MemoryStore, Store}

import scala.concurrent.duration.{FiniteDuration, SECONDS}

class CacheSpec extends IOFunSuite {

  trait MyTrait[F[_]] {

    def m1(key: String): F[Int]
    def m2(key: Int): F[String]

  }

  class MyService[F[_]: Sync] extends MyTrait[F] {

    var m1Count = 0
    var m2Count = 0

    override def m1(key: String): F[Int] = Sync[F].delay{
      m1Count += 1
      key.length
    }

    override def m2(key: Int): F[String] = Sync[F].delay{
      m2Count += 1
      key.toString
    }
  }

  def createCachedService[F[_]: Monad](service: MyTrait[F],
                                       store1: Store[F, String, Int],
                                       store2: Store[F, Int, String]): MyTrait[F] = new MyTrait[F] {
    override def m1(key: String): F[Int] =
      Cache(store1, service.m1).apply(key)

    override def m2(key: Int): F[String] =
      Cache(store2, service.m2).apply(key)
  }

  test("cache") = {
    for {
      store1 <- MemoryStore[IO, String, Int](None)
      store2 <- MemoryStore[IO, Int, String](None)
      service = new MyService[IO]()
      cachedService = createCachedService(service, store1, store2)
      val1  <- cachedService.m1("k")
      val2  <- cachedService.m1("k")
      val3  <- cachedService.m1("kk")
      val4  <- cachedService.m1("kk")

      val5  <- cachedService.m2(1)
      val6  <- cachedService.m2(1)
      val7  <- cachedService.m2(2)
      val8  <- cachedService.m2(2)
    } yield {
      assert(val1 == 1)
      assert(val2 == 1)
      assert(val3 == 2)
      assert(val4 == 2)
      assert(val5 == "1")
      assert(val6 == "1")
      assert(val7 == "2")
      assert(val8 == "2")
      assert(service.m1Count == 2)
      assert(service.m2Count == 2)
    }
  }

}