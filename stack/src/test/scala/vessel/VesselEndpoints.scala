package vessel

import cats.effect.{ConcurrentEffect, Resource, Sync, Timer}
import org.http4s.dsl.io.{->, GET, Root}
import org.http4s.server.{Router, Server}
import org.http4s.{HttpRoutes, Response, Status}

import scala.concurrent.ExecutionContext

/**
 * HTTP endpoints definitions for vessel.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class VesselEndpoints[F[_]: ConcurrentEffect: Timer](val pelle: Int) {

   def endpoints(): List[(String, HttpRoutes[F])] = {
      List(
         "vessel1" -> simpleVesselService1,
         "vessel2" -> simpleVesselService2
      )
   }

   def simpleVesselService1: HttpRoutes[F] = HttpRoutes.of[F] {
      case GET -> Root => Sync[F].pure {
         Response[F](Status.Ok).withEntity("i am simple vessel 1")
      }
   }

   def simpleVesselService2: HttpRoutes[F] = HttpRoutes.of[F] {
      case GET -> Root => Sync[F].pure {
         Response[F](Status.Ok).withEntity("i am simple vessel 2")
      }
   }


}