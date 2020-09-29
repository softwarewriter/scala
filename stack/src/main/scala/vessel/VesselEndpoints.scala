package vessel

import cats.effect.{ConcurrentEffect, Timer}
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

   val getByIMO: Complete = unsecure(
      Endpoint(
         "Get by IMO",
         Method.GET,
         Root / "vessel" / "imo".as[String],
         Produces.Directive.json[Vessel])
   ).run{imo =>
      vesselService.get(imo).toDirective(noSuchVessel(imo))
   }

   val putByIMO: Complete = unsecure(
      Endpoint(
         "Put by IMO",
         Method.PUT,
         Root / "vessel" / "imo".as[String],
         Accepts.json[Vessel],
         Produces.Directive.json[Vessel])
   ).run{case (imo, vessel) =>
      if (imo == vessel.imo) {
         Directive.success(vesselService.put(vessel))
      }
      else {
         //         Directive.error(Response[F](Status.BadRequest).withEntity(s"IMO-s not matching: $imo and ${vessel.imo}"))
         BadRequest(s"IMO-s not matching: $imo and ${vessel.imo}")
      }
   }

   val deleteByIMO: Complete = unsecure(
      Endpoint(
         "Delete by IMO",
         Method.DELETE,
         Root / "vessel" / "imo".as[String],
         Produces.Directive.json[Vessel])
   ).run{imo =>
      vesselService.delete(imo).toDirective(noSuchVessel(imo))
   }

   val search: Complete = unsecure(
      Endpoint(
         "Search",
         Method.GET,
         Root / "vessel" / "search" / "query".as[String],
         Produces.json[List[Vessel]])
   ).run(query => vesselService.search(query))

   def noSuchVessel(imo: String): Directive[F, Response[F]] = {
      Directive.error(Response[F](Status.NotFound).withEntity(s"No such vessel: $imo"))
      // NotFound
   }

   val routes: List[Complete] = List(getByIMO, putByIMO, deleteByIMO, search)

}
