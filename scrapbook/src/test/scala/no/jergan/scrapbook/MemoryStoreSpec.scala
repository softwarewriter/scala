package no.jergan.scrapbook

import cats.effect.{IO, Timer}
import no.jergan.scrapbook.RefCache.MemoryStore

import scala.concurrent.duration.{FiniteDuration, SECONDS}

class MemoryStoreSpec extends IOFunSuite {

  test("Test basic") = {
    for {
      store <- MemoryStore[IO, String, Int]()
      val1  <- store.get("k1")
      _     <- store.put("k1", 42)
      val2  <- store.get("k1")
      _     <- Timer[IO].sleep(FiniteDuration(1, SECONDS))
      val3  <- store.get("k2")
    } yield {
      assert(val1.isEmpty)
      assert(val2.contains(42))
      assert(val3.isEmpty)
    }
  }

  test("Test timeout") = {
    for {
      store <- MemoryStore[IO, String, Int](timeout = Some(100))
      val1  <- store.get("k1")
      _     <- store.put("k1", 42)
      _     <- store.put("k2", 43)
      val2  <- store.get("k1")
      val3  <- store.get("k2")
      _     <- store.put("k1", 44)
      val4  <- store.get("k1")
    } yield {
      assert(val1.isEmpty)
      assert(val2.contains(42))
      assert(val3.contains(43))
      assert(val4.contains(44))
    }
  }

  test("Test clear") = {
    for {
      store <- MemoryStore[IO, String, Int]()
      val1  <- store.get("k1")
      _     <- store.put("k1", 42)
      val2  <- store.get("k1")
      _     <- store.clear()
      val3  <- store.get("k1")
    } yield {
      assert(val1.isEmpty)
      assert(val2.contains(42))
      assert(val3.isEmpty)
    }
  }

}