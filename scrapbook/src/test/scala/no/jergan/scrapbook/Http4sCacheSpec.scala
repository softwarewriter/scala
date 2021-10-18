package no.jergan.scrapbook

import cats.effect.{Blocker, ConcurrentEffect, ExitCode, IO, Resource, Timer}
import ch.qos.logback.classic.Level
import io.circe.syntax.{EncoderOps, KeyOps}
import io.circe.{Decoder, Encoder, Json}
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.dsl.io.{->, /, GET, NotFound, Ok, Root}
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.Server
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes, Method, Request, Uri}
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import io.circe.generic.auto._
import io.circe.syntax._

import java.net.ServerSocket
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

class Http4sCacheSpec extends SharedResourceSpec {

  Logback.apply(Level.INFO)

  case class Person(firstName: String, lastName: String)

  implicit def entityEncoder[A: Encoder]: EntityEncoder[IO, A] = org.http4s.circe.jsonEncoderOf[IO, A]
  implicit def entityDecoder[A: Decoder]: EntityDecoder[IO, A] = org.http4s.circe.jsonOf[IO, A]

  private val httpPort = availablePort

  def availablePort: Int = {
    val server = new ServerSocket(0)
    val port   = server.getLocalPort
    server.close()
    port
  }

  case class FixtureParam(client: Client[IO], server: Server[IO])

  override def resource: Resource[IO, FixtureParam] = {
    val global = scala.concurrent.ExecutionContext.global
    for {
      client <- BlazeClientBuilder[IO](global).resource
      server <- TestServer.apply[IO](httpPort, global)
    } yield {
      FixtureParam(client, server)
    }
  }

  "test" - {
    "test2syn" in { fixture => {
      for {
        p <- fixture.client.expect[Person]("http://localhost/person")
      }
      yield {

      }
    }
    }
  }

  object TestServer {
    def apply[F[_]: ConcurrentEffect: Timer](port: Int, ec: ExecutionContext): Resource[IO, Server[IO]] = {

      def personService(id: String): Option[Person] = {
        val onkelSkrue  = new Person("Onkel", "Skrue")
        val onkelDonald = new Person("Donald", "Duck")
        val database = Map(
          "0" -> onkelSkrue,
          "1" -> onkelDonald
        )
        database.get(id)
      }

      val personHttpService: HttpRoutes[IO] = HttpRoutes.of[IO] {
        case GET -> Root / "person" / id =>
          personService(id) match {
            case None         => NotFound("No person with id " + id)
            case Some(person) => Ok(person)
          }
      }

      BlazeServerBuilder[IO](scala.concurrent.ExecutionContext.global)
        .bindHttp(port, "0.0.0.0")
        .enableHttp2(false)
        .withResponseHeaderTimeout(45.seconds)
        .withIdleTimeout(65.seconds)
        .withHttpApp(personHttpService.orNotFound)
        .resource
    }}

}
