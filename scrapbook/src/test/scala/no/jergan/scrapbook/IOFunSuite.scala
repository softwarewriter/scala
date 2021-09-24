package no.jergan.scrapbook

import cats.effect.{ContextShift, IO, Timer}
import org.scalatest.{Assertion, Tag}
import org.scalatest.funsuite.AsyncFunSuite

class IOFunSuite extends AsyncFunSuite {
  implicit def contextShift: ContextShift[IO] = IO.contextShift(executionContext)
  implicit def timer: Timer[IO]               = IO.timer(executionContext)
  object test {
    def update(name: String, assertion: IO[Assertion]): Unit =
      test(name, Tag("jergan")) { assertion.unsafeToFuture() }
  }
}
