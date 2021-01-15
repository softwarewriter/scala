package no.jergan.scrapbook

import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource}
import fs2._
import fs2.{Stream, io}

import java.nio.file.Paths
import _root_.io.circe.fs2._
import org.typelevel.jawn.AsyncParser
/*
  links
    https://github.com/circe/circe-iteratee
    https://github.com/circe/circe-fs2/blob/master/src/main/scala/io/circe/fs2/ParsingPipe.scala
    https://benfradet.github.io/blog/2017/07/29/a-small-guide-to-circe-fs2
    https://gist.github.com/BenFradet/a69e3fa86ad2654ed017e2cb2881c307
 */

object Memory2 extends IOApp {

//  val filename: String = "/Users/oyvind/tmp/persons.json"
  val filename: String = "/Users/oyvind/tmp/elements.json"

  def createApplication(): Resource[IO, Stream[IO, String]] = {
    for {
      blocker          <- Blocker[IO]
    } yield {
      val s: Stream[IO, String] = io.file.readAll[IO](Paths.get(filename), blocker, chunkSize = 4096)
        .through(text.utf8Decode)
      s
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    createApplication()
      .use((str: Stream[IO, String]) => {
        str
//          .evalTap(s => IO(println(s"chunk: $s")))
          .through(stringParser(AsyncParser.UnwrapArray))
          .evalTap(j => IO(println(s"json: $j")))
          .compile
          .lastOrError
          .map(_ => ExitCode.Success)
      })
  }

}
