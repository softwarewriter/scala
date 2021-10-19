package no.jergan.scrapbook

import cats.effect.{ExitCode, IO, IOApp}
import io.circe.{Encoder, Json, JsonObject}
import org.http4s.{HttpApp, HttpRoutes, Method, Request, Response, Status, Uri}
import org.http4s.dsl.io._
import org.http4s.implicits._
import io.circe.syntax._
import org.http4s.blaze.server.BlazeServerBuilder

/**
  * HTTP server for person.
  *
  * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
  */
object HTTPServer extends IOApp {

  case class Person(firstName: String, lastName: String, age: Int, uncle: Option[Person] = None)

  def personService(id: String): Option[Person] = {
    val onkelSkrue  = new Person("Onkel", "Skrue", 78)
    val onkelDonald = new Person("Donald", "Duck", 35, Some(onkelSkrue))
    val database = Map(
      "0" -> onkelSkrue,
      "1" -> onkelDonald,
      "2" -> new Person("Ole", "Duck", 9, Some(onkelDonald)),
      "3" -> new Person("Dole", "Duck", 8, Some(onkelDonald)),
      "4" -> new Person("Doffen", "Duck", 6, Some(onkelDonald)),
      "5" -> new Person("Nabo", "Jensen", 42),
    )
    database.get(id)
  }

  implicit val personEncoder: Encoder[Person] = (person: Person) => {
    val elements: List[(String, Json)] =
      List("firstName" -> Json.fromString(person.firstName),
         "lastName" -> Json.fromString(person.lastName),
         "age" -> Json.fromInt(person.age))
        .appendedAll(person.uncle.map(uncle => List("uncle" -> uncle.asJson)).getOrElse(Nil))
    Json.obj(elements: _*)

    Json
      .obj(
        "firstName" := person.firstName,
        "lastName" := person.lastName,
        "age" := person.age,
        "uncle" := person.uncle
      )
      .dropNullValues
  }

  implicit def entityEncoder[A: Encoder] = org.http4s.circe.jsonEncoderOf[IO, A]

  val personHttpService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "person" / id => {
      personService(id) match {
        case (None)         => NotFound("No person with id " + id)
        case (Some(person)) => Ok(person)
      }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    BlazeServerBuilder[IO](scala.concurrent.ExecutionContext.global)
      .bindHttp(8080)
      .withHttpApp(personHttpService.orNotFound)
      .serve
      .compile
      .drain
      .map(_ => ExitCode.Success)
  }

}
