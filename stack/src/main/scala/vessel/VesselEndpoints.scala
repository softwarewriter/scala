package vessel

import cats.effect.{ConcurrentEffect, Timer}
import io.unsecurity.UnsecurityOps
import io.unsecurity.hlinx.HLinx.{Root, _}
import io.unsecurity.hlinx.ParamConverter
import no.scalabin.http4s.directives.Directive
import org.http4s.{Method, Response, Status}

/**
 * Vessel endpoints using unsecurity.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class VesselEndpoints[F[_]: ConcurrentEffect: Timer](val unsecurity: ApplicationSecurity[F], val vesselService: VesselService[F]) extends UnsecurityOps[F] {

   import unsecurity._

  implicit val imoParamConverter = new ParamConverter[IMO] {
     override def convert(s: String): Either[String, IMO] = Right(IMO(s))
  }

   val routes: List[Complete] = List(getByIMO, putByIMO, deleteByIMO, search)

   def getByIMO: Complete = unsecure(
      Endpoint(
         "Get by IMO",
         Method.GET,
         Root / "vessel" / "imo".as[IMO],
         Produces.Directive.json[Vessel])
   ).run{imo =>
      Directive.liftF(vesselService.get(imo)).flatMap {
         case Some(vessel) => Directive.success(vessel)
         case None => noSuchVessel2(imo)
      }
   }

   def putByIMO: Complete = unsecure(
      Endpoint(
         "Put by IMO",
         Method.PUT,
         Root / "vessel" / "imo".as[IMO],
         Accepts.json[Vessel],
         Produces.Directive.json[Vessel])
   ).run{case (imo, vessel) =>
      if (imo == vessel.imo) {
         Directive.liftF(vesselService.put(vessel)).flatMap(vessel => Directive.success(vessel))
      }
      else {
         BadRequest(s"IMO-s not matching: $imo and ${vessel.imo}")
      }
   }

   def deleteByIMO: Complete = unsecure(
      Endpoint(
         "Delete by IMO",
         Method.DELETE,
         Root / "vessel" / "imo".as[IMO],
         Produces.Directive.json[Vessel])
   ).run{imo =>
      Directive.liftF(vesselService.delete(imo)).flatMap {
         case Some(vessel) => Directive.success(vessel)
         case None => noSuchVessel2(imo)
      }
   }

   def search: Complete = unsecure(
      Endpoint(
         "Search",
         Method.GET,
         Root / "vessel" / "search" / "query".as[String],
         Produces.Directive.json[List[Vessel]])
   ).run{query =>
      Directive.liftF(vesselService.search(query)).flatMap(vessels => Directive.success(vessels))
   }

   def noSuchVessel(imo: IMO): Directive[F, Response[F]] = {
      Directive.error(Response[F](Status.NotFound).withEntity(s"No such vessel: $imo"))
   }

   def noSuchVessel2(imo: IMO): Directive[F, Vessel] = {
      Directive.error(Response[F](Status.NotFound).withEntity(s"No such vessel: $imo"))
   }
}
