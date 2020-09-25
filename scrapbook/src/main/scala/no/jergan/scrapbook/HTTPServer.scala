package no.jergan.scrapbook

import cats.effect.{ExitCode, IO, IOApp}
import io.circe.{Encoder, Json}
import org.http4s.{HttpApp, HttpRoutes, Method, Request, Response, Status, Uri}
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import io.circe.syntax._

/**
  * HTTP server for person.
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
      "4" -> Person("Nabo", "Jensen", 6),
    )
    database.get(id)
  }

  implicit val personEncoder: Encoder[Person] = new Encoder[Person] {
    def apply(person: Person): Json = Json.obj(
      ("firstName", Json.fromString(person.firstName)),
      ("lastName", Json.fromString(person.lastName)),
      ("age", Json.fromInt(person.age))
    )
  }

  val personHttpService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "person" / id => {
      val personOrNot = personService(id)
      (personOrNot) match {
        case (None)         => NotFound("No person with id " + id)
        case (Some(person)) => Ok(person.asJson.toString())
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
