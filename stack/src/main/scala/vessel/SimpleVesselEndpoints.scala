package vessel

import cats.effect.{ConcurrentEffect, Sync}
import org.http4s.dsl.io.{->, GET, Root}
import org.http4s.{HttpRoutes, Response, Status}

/**
 * HTTP endpoints definitions for vessel defined without using unsecurity.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class SimpleVesselEndpoints[F[_]: ConcurrentEffect] {

   def endpoints(path: String): List[(String, HttpRoutes[F])] = {
      List(
         path + "/vessel1" -> simpleVesselService1,
         path + "/vessel2" -> simpleVesselService2
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