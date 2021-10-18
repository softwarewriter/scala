package no.jergan.scrapbook

import cats.effect.{ContextShift, IO, Resource, Timer}
import org.scalatest.freespec.FixtureAsyncFreeSpec
import org.scalatest.{Assertion, BeforeAndAfterAll, FutureOutcome}

import scala.concurrent.Future
import scala.language.implicitConversions

abstract class SharedResourceSpec extends FixtureAsyncFreeSpec with BeforeAndAfterAll {
  implicit def contextShift: ContextShift[IO]                   = IO.contextShift(scala.concurrent.ExecutionContext.global)
  implicit def timer: Timer[IO]                                 = IO.timer(scala.concurrent.ExecutionContext.global)
  implicit def IOToFuture(io: IO[Assertion]): Future[Assertion] = io.unsafeToFuture()
  implicit def IOUnitToFuture(io: IO[Unit]): Future[Assertion]  = io.map(_ => assert(true)).unsafeToFuture()

  def resource: Resource[IO, FixtureParam]

  private var allocatedResource: Option[FixtureParam] = None
  private var cleaner: IO[Unit]                       = IO.unit

  override protected def beforeAll(): Unit = {
    val tpl = resource.allocated.unsafeRunSync()
    allocatedResource = Some(tpl._1)
    cleaner = tpl._2
  }

  override protected def afterAll(): Unit = cleaner.unsafeRunSync()

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withFixture(test.toNoArgAsyncTest(allocatedResource.get))
  }
}
