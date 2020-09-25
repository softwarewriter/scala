package no.jergan.scrapbook

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.client.{Client, JavaNetClientBuilder}

/**
  * What does this class do?
  *
  * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
  */
object HTTPClient extends IOApp {

  /*
 import org.http4s.client.blaze._
 import org.http4s.client._
 import scala.concurrent.ExecutionContext.global

 BlazeClientBuilder[IO](global).resource.use { client =>
    // use `client` here and return an `IO`.
    // the client will be acquired and shut down
    // automatically each time the `IO` is run.
    IO.unit
 }

   */

  import cats.effect.Blocker
  import java.util.concurrent._

  override def run(args: List[String]): IO[ExitCode] = {
    /*
    val blockingPool = Executors.newFixedThreadPool(5)
    val blocker = Blocker.liftExecutorService(blockingPool)
    val httpClient: Client[IO] = JavaNetClientBuilder[IO](blocker).create

    val service: IO[String] =
      httpClient.expect[String]("http://localhost:1337/echo/pelle")

    val people = Vector("ole", "dole", "doff")
    // people: scala.collection.immutable.Vector[String] = Vector(Michael, Jessica, Ashley, Christopher)

    val greetingList: IO[Vector[String]] = people.mmap(service)

    println(service.unsafeRunSync)
    IO(ExitCode.Success)
  }

     */

    /*
    val strings: List[String] = List("ole", "dole", "doffen")

    val size: (String => Int) = s => s.length
    val doubleSize: (String => Int) = size(_) * 2

    val ints: List[Int] = List(1, 2, 3)

    val ints2: List[Int] = for (i <- ints) yield i

    val generate: (Int => List[String]) = (a: Int) => {
      val s: String = "a"
      for(i <- 1 until a) yield s
    }


    val sizes: List[String] = strings.map(size).map(generate)

    println(sizes);
    val functions: List[String => Int] = List(size, doubleSize)


    println(functions(1)("pelle"))

     */
    IO(ExitCode.Success)

  }

}
