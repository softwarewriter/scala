package no.jergan.scrapbook

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.{HttpApp, HttpRoutes, Method, Request, Response, Status, Uri}
import org.http4s.dsl.io._
import org.http4s.implicits._
import cats.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder

/**
 * What does this class do?
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object HTTPServer extends IOApp {

   case class Person(firstName: String, lastName: String, age: Int)

   def personService(id: String): Option[Person] = {
      val database = Map(
         "1" -> Person("Ole", "Duck", 9),
         "2" -> Person("Dole", "Duck", 8),
         "3" -> Person("Doffen", "Duck", 6),
      )
      database.get(id)
   }

   val personHttpService: HttpRoutes[IO] = HttpRoutes.of[IO]{
      case GET -> Root / "person" / id => {
        val personOrNot = personService(id)
         (personOrNot) match {
            case (None) => NotFound("No person with id " + id)
            case (Some(person)) => Ok(person.firstName)
         }
      }
   }

   override def run(args: List[String]): IO[ExitCode] = {
      BlazeServerBuilder[IO](scala.concurrent.ExecutionContext.global)
         .bindHttp(1337)
         .withHttpApp(personHttpService.orNotFound)
         .serve
         .compile
         .drain
         .map(_ => ExitCode.Success)
   }

}

