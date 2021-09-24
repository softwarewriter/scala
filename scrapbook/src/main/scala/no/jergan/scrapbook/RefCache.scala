package no.jergan.scrapbook

import cats.Applicative
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Resource, Sync, Timer}
import cats.implicits._
import fs2.{RaiseThrowable, Stream}
import no.jergan.scrapbook.RefCache.MemoryCache.Entry

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration, SECONDS}

/**
  * What does this class do?
  *
  * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
  */
object RefCache extends IOApp {

  trait Cache[F[_], K, V] {

    def get(key: K): F[Option[V]]
    def put(key: K, value: V): F[Unit]

  }

  class MemoryCache[F[_]: Sync: Timer, K, V](ref: Ref[F, Map[K, Entry[V]]], timeout: Long) extends Cache[F, K, V] {

    override def get(key: K): F[Option[V]] =
      for {
        now     <- Timer[F].clock.monotonic(TimeUnit.MILLISECONDS)
        current <- ref.get
        entry   = current.get(key)
        value <- entry match {
                  case None                              => Applicative[F].pure(None)
                  case Some(e) if e.time < now - timeout => ref.set(current.removed(key)).map(_ => None)
                  case Some(e)                           => Applicative[F].pure(Some(e.value))
                }
      } yield {
        value
      }

    override def put(key: K, value: V): F[Unit] =
      for {
        now     <- Timer[F].clock.monotonic(TimeUnit.MILLISECONDS)
        current <- ref.get
        _       <- ref.set(current + (key -> Entry(value, now)))
      } yield {}

  }

  object MemoryCache {

    case class Entry[V](value: V, time: Long)

    def apply[F[_]: Sync: Timer, K, V](timeout: Long): F[Cache[F, K, V]] =
      Ref
        .of[F, Map[K, Entry[V]]](Map())
        .map(map => new MemoryCache[F, K, V](map, timeout))

  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      cache <- MemoryCache[IO, String, Int](42)
      val1  <- cache.get("k1")
      _     <- cache.put("k1", 42)
      _     <- cache.put("k2", 43)
      val2  <- cache.get("k1")
      val3  <- cache.get("k2")
      _     <- cache.put("k1", 44)
      val4  <- cache.get("k1")
      _     <- Timer[IO].sleep(FiniteDuration(1, SECONDS))
      val5  <- cache.get("k1")

    } yield {
      println(val1)
      println(val2)
      println(val3)
      println(val4)
      println(val5)
      ExitCode.Success
    }
  }

}
