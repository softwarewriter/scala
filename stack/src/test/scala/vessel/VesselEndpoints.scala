package vessel

import io.unsecurity.hlinx.HLinx.Root
import io.unsecurity.hlinx.HLinx._
import org.http4s.Method
import cats.effect.{ConcurrentEffect, Timer}
import io.unsecurity.Server.toHttpRoutes
import io.unsecurity.UnsecurityOps

/**
 * Vessel endpoints using unsecurity.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class VesselEndpoints[F[_]: ConcurrentEffect: Timer](val unsecurity: ApplicationSecurity[F], val vesselService: VesselService) extends UnsecurityOps[F] {

   import unsecurity._

   def endpoints(path: String): List[(String, org.http4s.HttpRoutes[F])] = {
      List(
         path + "/" -> toHttpRoutes(getByIMO),
         path + "/put/" -> toHttpRoutes(putByIMO)
      )
   }

   // Directives

   val getByIMO: Complete = unsecure(
      Endpoint(
         "Get by IMO",
         Method.GET,
         Root / "imo".as[String],
         Produces.json[Option[Vessel]])
   ).run(imo => vesselService.get(imo))

   val putByIMO: Complete = unsecure(
      Endpoint(
         "Put by IMO",
         Method.PUT,
         Root / "imo".as[String],
         Accepts.json[Vessel],
         Produces.json[Vessel])
   ).run(tuple => {
      val vessel = tuple._2
      vesselService.put(vessel)
      vessel
   })

}
