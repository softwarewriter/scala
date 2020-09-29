package vessel

import cats.effect.{ConcurrentEffect, IO, Resource, Sync, Timer}
import org.http4s.{HttpRoutes, Response, Status}
import org.http4s.dsl.io.{->, /, GET, NotFound, Ok, Root}
import org.http4s.server.{Router, Server}

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

      val vesselEndpoints = new VesselEndpoints[F](42)
      val simpleVessel: HttpRoutes[F] = vesselEndpoints.simpleVesselService
      val routes = Router(
         "/health" -> health,
         "/simple"-> simpleHttpService,
         "/vessel"-> simpleVessel
      )

      platform.HttpServer[F](configuration.port, configuration.bindAddress, executionContext, routes)
   }

}