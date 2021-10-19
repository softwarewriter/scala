package no.jergan.scrapbook

import cats.effect.{ConcurrentEffect, IO, Resource, Timer}
import cats.implicits.toSemigroupKOps
import ch.qos.logback.classic.Level
import io.chrisdavenport.mules.http4s.{CacheItem, CacheMiddleware, CacheType}
import io.chrisdavenport.mules.caffeine._
import io.circe.{Decoder, Encoder}
import org.http4s.client.Client
import org.http4s.dsl.io.{->, /, GET, NotFound, Ok, Root}
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.Server
import org.http4s.{CacheDirective, EntityDecoder, EntityEncoder, HttpRoutes, Method, Status, Uri}
import org.http4s.dsl.io._
import org.http4s.implicits._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.middleware.Caching

import java.net.ServerSocket
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

class Http4sCacheSpec extends SharedResourceSpec {

  Logback.apply(Level.INFO)

  case class Person(firstName: String, lastName: String)

  class Service() {
    var count = 0;

    val one  = new Person("Person", "One")
    val two = new Person("Person", "Two")
    val database = Map(
      "1" -> one,
      "2" -> two
    )

    def person(id: String): Option[Person] = {
      count += 1
      database.get(id)
    }
  }

  implicit def entityEncoder[A: Encoder]: EntityEncoder[IO, A] = org.http4s.circe.jsonEncoderOf[IO, A]
  implicit def entityDecoder[A: Decoder]: EntityDecoder[IO, A] = org.http4s.circe.jsonOf[IO, A]

  private val httpPort = availablePort

  def availablePort: Int = {
    val server = new ServerSocket(0)
    val port   = server.getLocalPort
    server.close()
    port
  }

  case class FixtureParam(client: Client[IO], service: Service)

  override def resource: Resource[IO, FixtureParam] = {
    val global = scala.concurrent.ExecutionContext.global
    for {
      cache <- Resource.eval(CaffeineCache.build[IO, (Method, Uri), CacheItem](None, None, Some(10000L)))
      cacheMiddleware = CacheMiddleware.client(cache, CacheType.Public)
      client  <- BlazeClientBuilder[IO](global).resource
      cachedClient = cacheMiddleware(client)
      service = new Service()
      _       <- TestServer.apply[IO](httpPort, global, service)
    } yield {
      FixtureParam(cachedClient, service)
    }
  }

  "test" - {
    "testCache" in { fixture => {
      for {
        c11 <- fixture.client.expect[Person](cached("1"))
        c12 <- fixture.client.expect[Person](cached("1"))
        c21 <- fixture.client.expect[Person](cached("2"))
        c31 <- fixture.client.statusFromUri(cached("3"))
        c32 <- fixture.client.statusFromUri(cached("3"))

        nc10 <- fixture.client.expect[Person](nonCached("1"))
        nc11 <- fixture.client.expect[Person](nonCached("1"))
      }
      yield {
        assert(c11.lastName == "One")
        assert(c12.lastName == "One")
        assert(c21.lastName == "Two")
        assert(c31 == Status.NotFound)
        assert(c32 == Status.NotFound)

        assert(nc10.lastName == "One")
        assert(nc11.lastName == "One")

        assert(fixture.service.count == 5)
      }
    }
    }
  }

  object TestServer {
    def apply[F[_]: ConcurrentEffect: Timer](port: Int, ec: ExecutionContext, service: Service): Resource[IO, Server] = {

      val cachedService: HttpRoutes[IO] = HttpRoutes.of[IO] {
        case GET -> Root / "cached" / id =>
          service.person(id) match {
            case None         => NotFound("No person with id " + id)
            case Some(person) => Ok(person)
          }
      }
      val nonCachedService: HttpRoutes[IO] = HttpRoutes.of[IO] {
        case GET -> Root / "noncached" / id =>
          service.person(id) match {
            case None         => NotFound("No person with id " + id)
            case Some(person) => Ok(person)
          }
      }
      val cached: HttpRoutes[IO] = Caching.publicCache(5.minutes, cachedService)
      val nonCached: HttpRoutes[IO] = Caching.`no-store`(nonCachedService)
      val allRoutes: HttpRoutes[IO] = cached <+> nonCached

      BlazeServerBuilder[IO](scala.concurrent.ExecutionContext.global)
        .bindHttp(port, "0.0.0.0")
        .enableHttp2(false)
        .withResponseHeaderTimeout(45.seconds)
        .withIdleTimeout(65.seconds)
        .withHttpApp(allRoutes.orNotFound)
        .resource
    }}

  private def cached(id: String): Uri = Uri.unsafeFromString(s"http://localhost:$httpPort/cached/$id")
  private def nonCached(id: String): Uri = Uri.unsafeFromString(s"http://localhost:$httpPort/noncached/$id")

}
