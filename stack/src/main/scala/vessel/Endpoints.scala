package vessel

import cats.effect.{ConcurrentEffect, Resource, Sync, Timer}
import org.http4s.dsl.io.{->, GET, Root}
import org.http4s.server.{Router, Server}
import org.http4s.{HttpRoutes, Response, Status}
import io.unsecurity.Server.toHttpRoutes

import scala.concurrent.ExecutionContext

/**
 * HTTP endpoints definitions.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object Endpoints {

   def create[F[_]: ConcurrentEffect: Timer](configuration: Configuration, executionContext: ExecutionContext, vesselService: VesselService[F]): Resource[F, Server[F]] = {

      val health: HttpRoutes[F] = platform.HttpServer.healthCheck[F](mountOnRoot = true)
      val simpleHttpService: HttpRoutes[F] = HttpRoutes.of[F] {
         case GET -> Root => Sync[F].pure {
            Response[F](Status.Ok).withEntity("i am simple")
         }
      }

      val unsecurity: ApplicationSecurity[F] = platform.UnsecurityInstances.m2m("issuer", "whoami", user => ConcurrentEffect[F].delay(Option(user)))

      val simpleVesselEndpoints = new SimpleVesselEndpoints[F]
      val vesselEndpoints = new VesselEndpoints[F](unsecurity, vesselService)
      val routes = Router(
         "/health" -> health,
         "/simple"-> simpleHttpService,
         "/" -> toHttpRoutes(vesselEndpoints.routes),
        "/simplevessel" -> simpleVesselEndpoints.endpoints
      )

      platform.HttpServer[F](configuration.port, configuration.bindAddress, executionContext, routes)
   }

}
