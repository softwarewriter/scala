package vessel

import cats.effect.{ConcurrentEffect, Resource, Sync, Timer}
import org.http4s.dsl.io.{->, GET, Root}
import org.http4s.server.{Router, Server}
import org.http4s.{HttpRoutes, Response, Status}

import scala.concurrent.ExecutionContext

/**
 * HTTP endpoints definitions.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object Endpoints {

   def create[F[_]: ConcurrentEffect: Timer](configuration: Configuration, executionContext: ExecutionContext): Resource[F, Server[F]] = {

      val health: HttpRoutes[F] = platform.HttpServer.healthCheck[F](mountOnRoot = true)
      val simpleHttpService: HttpRoutes[F] = HttpRoutes.of[F] {
         case GET -> Root => Sync[F].pure {
            Response[F](Status.Ok).withEntity("i am simple")
         }
      }

      val unsecurity: ApplicationSecurity[F] = platform.UnsecurityInstances.m2m("issuer", "whoami", user => ConcurrentEffect[F].delay(Option(user)))

      val simpleVesselEndpoints = new SimpleVesselEndpoints[F]
      val vesselEndpoints = new VesselEndpoints[F](unsecurity, configuration.vesselService)
      val routes = List(
         "/health" -> health,
         "/simple"-> simpleHttpService)
         .appendedAll(simpleVesselEndpoints.endpoints("/simplevessel"))
         .appendedAll(vesselEndpoints.endpoints("/vessel"))

      platform.HttpServer[F](configuration.port, configuration.bindAddress, executionContext, Router(routes:_*))
   }

}
