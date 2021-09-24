package no.jergan.scrapbook

import cats.{Applicative, Monad}
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Resource, Sync, Timer}
import cats.implicits._
import fs2.{RaiseThrowable, Stream}
import no.jergan.scrapbook.RefCache.MemoryStore.Entry

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration, SECONDS}

/**
 * What does this class do?
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object RefCache {

  trait Store[F[_], K, V] {

    def get(key: K): F[Option[V]]
    def put(key: K, value: V): F[Unit]

  }

  class MemoryStore[F[_]: Sync: Timer, K, V](ref: Ref[F, Map[K, Entry[V]]], timeout: Option[Long]) extends Store[F, K, V] {

    override def get(key: K): F[Option[V]] =
      for {
        now     <- Timer[F].clock.monotonic(TimeUnit.MILLISECONDS)
        current <- ref.get
        entry   = current.get(key)
        value   <- entry match {
          case None                              => Applicative[F].pure(None)
          case Some(e) if timeout.fold(false)(e.time < now - _) => ref.set(current.removed(key)).map(_ => None)
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

  object MemoryStore {

    case class Entry[V](value: V, time: Long)

    def apply[F[_]: Sync: Timer, K, V](timeout: Option[Long]): F[Store[F, K, V]] =
      Ref
        .of[F, Map[K, Entry[V]]](Map())
        .map(new MemoryStore[F, K, V](_, timeout))

  }

  object Cache {

    def apply[F[_]: Monad, K, V](store: Store[F, K, V], f: K => F[V]): K => F[V] = k =>
      for {
        storedValue <- store.get(k)
        result <- storedValue match {
          case Some(v) => Applicative[F].pure(v)
          case None => f(k).flatMap(functionValue => store.put(k, functionValue).map(_ => functionValue))
        }
      }
      yield {
        result
      }

  }

}
