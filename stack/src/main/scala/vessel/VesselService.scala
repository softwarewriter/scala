package vessel

import cats.Applicative

import scala.collection.mutable._

/**
 * Definition of vessel service
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
trait VesselService[F[_]] {

   def get(imo: IMO): F[Option[Vessel]]

   def put(vessel: Vessel): F[Vessel]

   def delete(imo: IMO): F[Option[Vessel]]

   def search(query: String): F[List[Vessel]]
}

class SimpleVesselService[F[_]](implicit F: Applicative[F]) extends VesselService[F] {

   private val storage: Map[IMO, Vessel] = new HashMap[IMO, Vessel]()

   override def get(imo: IMO): F[Option[Vessel]] = {
      F.pure(storage.get(imo))
   }

   override def put(vessel: Vessel): F[Vessel] = {
      storage.put(vessel.imo, vessel)
      F.pure(vessel)
   }

   override def delete(imo: IMO): F[Option[Vessel]] = {
      F.pure(storage.remove(imo))
   }

   override def search(query: String): F[List[Vessel]] = {
      F.pure(storage.values.filter(vessel => vessel.imo.value.contains(query) || vessel.name.contains(query)).toList.sortBy(vessel => vessel.imo.value))
   }

   put(Vessel(IMO("1"), "Titanic"))
   put(Vessel(IMO("2"), "Norge"))
   put(Vessel(IMO("3"), "Eidsvold"))

}
