package no.jergan.scrapbook

import cats.effect.{Concurrent, ExitCode, IO, IOApp, Sync}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

/**
 * Cats.
 */
object CatsIOTimeout extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    val io = IO {
      println("pelle1")
      Thread.sleep(3000)
      println("pelle2")
    }
    val io2 = IO {
      try {
        Concurrent.timeout(io, Duration(1, TimeUnit.SECONDS))
      }
      catch {
        case e: NullPointerException => println(e)
      }
    }
    io2.map(_ => ExitCode.Success)
  }

}
