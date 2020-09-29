package vessel

import cats.effect.{ConcurrentEffect, Timer}
import io.unsecurity.Server.toHttpRoutes
import io.unsecurity.UnsecurityOps
import io.unsecurity.hlinx.HLinx.{Root, _}
import no.scalabin.http4s.directives.Directive
import org.http4s.{Method, Response, Status}

/**
 * Vessel endpoints using unsecurity.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class VesselEndpoints[F[_]: ConcurrentEffect: Timer](val unsecurity: ApplicationSecurity[F], val vesselService: VesselService) extends UnsecurityOps[F] {

   import unsecurity._

   def endpoints(path: String): List[(String, org.http4s.HttpRoutes[F])] = {
      List(
         path + "/" -> toHttpRoutes(getByIMO.merge(putByIMO).merge(deleteByIMO)),
         path + "/search" -> toHttpRoutes(search)
      )
   }

   val getByIMO: Complete = unsecure(
      Endpoint(
         "Get by IMO",
         Method.GET,
         Root / "imo".as[String],
         Produces.Directive.json[Vessel])
   ).run(imo => {
      vesselService.get(imo) match {
         case Some(vessel) => Directive.success(vessel)
         case None => noSuchVessel(imo)
      }
   })

   val putByIMO: Complete = unsecure(
      Endpoint(
         "Put by IMO",
         Method.PUT,
         Root / "imo".as[String],
         Accepts.json[Vessel],
         Produces.Directive.json[Vessel])
   ).run(tuple => {
      val imo = tuple._1
      val vessel = tuple._2
      if (imo == vessel.imo)
      {
         vesselService.put(vessel)
         Directive.success(vessel)
      }
      else {
         Directive.error(Response[F](Status.BadRequest).withEntity(s"IMO-s not matching: $imo and ${vessel.imo}"))
      }
   })

   val deleteByIMO: Complete = unsecure(
      Endpoint(
         "Delete by IMO",
         Method.DELETE,
         Root / "imo".as[String],
         Produces.Directive.json[Vessel])
   ).run(imo => {
      vesselService.delete(imo) match {
         case Some(vessel) => Directive.success(vessel)
         case None => noSuchVessel(imo)
      }
   })

   val search: Complete = unsecure(
      Endpoint(
         "Search",
         Method.GET,
         Root / "query".as[String],
         Produces.json[List[Vessel]])
   ).run(query => vesselService.search(query))

  def noSuchVessel[A](imo: String): Directive[F, A] = {
     Directive.error(Response[F](Status.NotFound).withEntity(s"No such vessel: $imo"))
  }

}
