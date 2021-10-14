package no.jergan.scrapbook

import cats.{Applicative, Monad}
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Resource, Sync, Timer}
import cats.implicits._
import fs2.{RaiseThrowable, Stream}
import no.jergan.scrapbook.RefCache.MemoryStore.{Entry, State}

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
    def clear(): F[Unit]

  }

  class MemoryStore[F[_]: Sync: Timer, K, V](ref: Ref[F, State[K, V]],
                                             maxSize: Option[Long],
                                             timeout: Option[Long]) extends Store[F, K, V] {

    override def get(key: K): F[Option[V]] =
      for {
        now <- Timer[F].clock.monotonic(TimeUnit.MILLISECONDS)
        value <- ref.modify(state => state.get(key, now, timeout))
      } yield value

    override def put(key: K, value: V): F[Unit] =
      for {
        now     <- Timer[F].clock.monotonic(TimeUnit.MILLISECONDS)
        _       <- ref.update(state => state.put(key, value, now))
      } yield {}

    override def clear(): F[Unit] =
      for {
        _       <- ref.set(State())
      } yield {}

  }

  object MemoryStore {

    case class State[K, V](map: Map[K, Entry[V]],
                           oldest: Option[Entry[V]],
                           newest: Option[Entry[V]]) {

      def get(key: K, now: Long, timeout: Option[Long]): (State[K, V], Option[V]) =
        map.get(key) match {
          case None                                             => (this, None)
          case Some(e) if timeout.fold(false)(e.time < now - _) => (State(map.removed(key), None, None), None)
          case Some(e)                                          => (this, Some(e.value))
        }

      def put(key: K, value: V, now: Long): State[K, V] =
        State(map + (key -> Entry(None, value, now, None)), None, None)
    }

    object State {

      def apply[K, V](): State[K, V] = new State(Map.empty, None, None)
    }

    case class Entry[V](older: Option[Entry[V]], value: V, time: Long, newer: Option[Entry[V]])

    def apply[F[_]: Sync: Timer, K, V](maxSize: Option[Long] = None, timeout: Option[Long] = None): F[Store[F, K, V]] =
      Ref
        .of[F, State[K, V]](State())
        .map(new MemoryStore[F, K, V](_, maxSize, timeout))

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
