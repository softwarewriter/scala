package vessel

import cats.effect.{ConcurrentEffect, Sync}
import org.http4s.dsl.io._
import org.http4s.{HttpRoutes, Response, Status}

import cats.implicits._

/**
 * HTTP endpoints definitions for vessel defined without using unsecurity.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class SimpleVesselEndpoints[F[_]: ConcurrentEffect] {

   val endpoints: HttpRoutes[F] = {
     simpleVesselService1 <+> simpleVesselService2
   }

   def simpleVesselService1: HttpRoutes[F] = HttpRoutes.of[F] {
      case GET -> Root / "vessel1" => Sync[F].pure {
         Response[F](Status.Ok).withEntity("i am simple vessel 1")
      }
   }

   def simpleVesselService2: HttpRoutes[F] = HttpRoutes.of[F] {
      case GET -> Root / "vessel2" => Sync[F].pure {
         Response[F](Status.Ok).withEntity("i am simple vessel 2")
      }
   }


}