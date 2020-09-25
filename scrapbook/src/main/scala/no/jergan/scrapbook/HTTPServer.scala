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

  case class Person(firstName: String, lastName: String, age: Int, uncle: Option[Person] = None)

  object Person {
     implicit def personToOption(person: Person) = Some(person)
  }

  def personService(id: String): Option[Person] = {
    val onkelSkrue  = new Person("Onkel", "Skrue", 78)
    val onkelDonald = new Person("Donald", "Duck", 35, onkelSkrue)
    val database = Map(
      "0" -> onkelSkrue,
      "1" -> onkelDonald,
      "2" -> new Person("Ole", "Duck", 9, onkelDonald),
      "3" -> new Person("Dole", "Duck", 8, onkelDonald),
      "4" -> new Person("Doffen", "Duck", 6, onkelDonald),
      "5" -> new Person("Nabo", "Jensen", 42),
    )
    database.get(id)
  }

  // @kaare: hvordan (elegant) lage en variabel liste av elementer (slik at jeg slipper "no uncle").
  implicit val personEncoder: Encoder[Person] = (person: Person) =>
    Json.obj(
      ("firstName", Json.fromString(person.firstName)),
      ("lastName", Json.fromString(person.lastName)),
      ("age", Json.fromInt(person.age)),
      person.uncle match {
        case Some(uncle) => ("uncle", uncle.asJson)
        case None        => ("no uncle", Json.fromString(""))
      }
  )

  val personHttpService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "person" / id => {
      personService(id) match {
        case (None)         => NotFound("No person with id " + id)
        case (Some(person)) => Ok(person.asJson.toString())
      }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    BlazeServerBuilder[IO](scala.concurrent.ExecutionContext.global)
      .bindHttp(6510)
      .withHttpApp(personHttpService.orNotFound)
      .serve
      .compile
      .drain
      .map(_ => ExitCode.Success)
  }

}
