package vessel

import io.unsecurity.hlinx.HLinx.Root
import io.unsecurity.hlinx.HLinx._
import org.http4s.Method

import cats.effect.{ConcurrentEffect, Timer}
import io.unsecurity.Server.toHttpRoutes

/**
 * Vessel endpoints using unsecurity.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class VesselEndpoints[F[_]: ConcurrentEffect: Timer](val unsecurity: ApplicationSecurity[F]) {

   import unsecurity._

   def endpoints(path: String): List[(String, org.http4s.HttpRoutes[F])] = {
      List(
         path + "/" -> toHttpRoutes(getVessel)
//         path + "/" -> toHttpRoutes(getVessel)
      )
   }

   val getVessel: Complete = unsecure(
      Endpoint(
         "Get by IMO",
         Method.GET,
         Root / "vessel" / "imo".as[String],
         Produces.json[String])
   ).run(IMO => "Titanic")

}
