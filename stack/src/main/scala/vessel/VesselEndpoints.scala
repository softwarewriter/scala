package vessel

import cats.Monad
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
class VesselEndpoints[F[_]: ConcurrentEffect: Timer](val unsecurity: ApplicationSecurity[F], val vesselService: VesselService[F]) extends UnsecurityOps[F] {

   import unsecurity._

   val routes: List[Complete] = List(getByIMO, putByIMO, deleteByIMO, search)

   def getByIMO: Complete = unsecure(
      Endpoint(
         "Get by IMO",
         Method.GET,
         Root / "vessel" / "imo".as[String],
         Produces.Directive.json[Vessel])
   ).run{imo =>
      Directive.liftF(vesselService.get(imo)).flatMap((maybyVessel: Option[Vessel]) => {
         maybyVessel match {
            case Some(vessel) => Directive.success(vessel)
//            case None => noSuchVessel(imo)
            case None => Directive.error(Response[F](Status.BadRequest).withEntity(s"No such imo: $imo"))
         }
      } )
      //     v2
//     val v3: Directive[F, Directive[F, Option[Vessel]]] = Directive.success(v2)
//     val v2 = v1.toDirective.orElse()
//     v2
     /*


//     val v2: Directive[F, F[Option[Vessel]]] = Directive.pure(v1)
//      val v1: F[Option[Vessel]] =
     val v3: Directive[F, F[Option[Vessel]]] = Directive.success(v1)
     v3

      */
   }

   def putByIMO: Complete = unsecure(
      Endpoint(
         "Put by IMO",
         Method.PUT,
         Root / "vessel" / "imo".as[String],
         Accepts.json[Vessel],
         Produces.Directive.json[Vessel])
   ).run{case (imo, vessel) => ??? /*
      if (imo == vessel.imo) {
         Directive.success(vesselService.put(vessel))
      }
      else {
                  Directive.error(Response[F](Status.BadRequest).withEntity(s"IMO-s not matching: $imo and ${vessel.imo}"))
         BadRequest(s"IMO-s not matching: $imo and ${vessel.imo}")
      }
      */
   }

   def deleteByIMO: Complete = unsecure(
      Endpoint(
         "Delete by IMO",
         Method.DELETE,
         Root / "vessel" / "imo".as[String],
         Produces.Directive.json[Vessel])
   ).run{imo => ??? }
  /*
      val v1: F[Option[Vessel]] = vesselService.delete(imo)
     Directive.getOrElseF(v1, x)
     toDirective()
     Directive.getOrElse(v1)
//      val v2: Option[Vessel] = ???
//      v2.toDirective(noSuchVessel(imo))
   }

   */

   def search: Complete = unsecure(
      Endpoint(
         "Search",
         Method.GET,
         Root / "vessel" / "search" / "query".as[String],
         Produces.Directive.json[List[Vessel]])
   ).run{query => ??? /*
      Directive.success(vesselService.search(query))
      */
   }

   def noSuchVessel(imo: String): Directive[F, Response[F]] = {
      Directive.error(Response[F](Status.NotFound).withEntity(s"No such vessel: $imo"))
   }

   def noSuchVessel2(imo: String): Directive[F, Response[F]] = {
      Directive.error(Response[F](Status.NotFound).withEntity(s"No such vessel: $imo"))
   }
}
