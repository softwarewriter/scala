package vessel

import scala.collection.mutable._

/**
 * Definition of vessel service
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
trait VesselService[F[_]] {

   def get(imo: String): F[Option[Vessel]]

   def put(vessel: Vessel): F[Vessel]

   def delete(imo: String): F[Option[Vessel]]

   def search(query: String): F[List[Vessel]]
}

object SimpleVesselService extends VesselService[Option] {

   private val storage: Map[String, Vessel] = new HashMap[String, Vessel]()

   override def get(imo: String): Option[Option[Vessel]] = {
      Option(storage.get(imo))
   }

   override def put(vessel: Vessel): Option[Vessel] = {
      storage.put(vessel.imo, vessel)
      Option(vessel)
   }

   override def delete(imo: String): Option[Option[Vessel]] = {
      Option(storage.remove(imo))
   }

   override def search(query: String): Option[List[Vessel]] = {
      Option(storage.values.filter(vessel => vessel.imo.contains(query) || vessel.name.contains(query)).toList.sortBy(vessel => vessel.imo))
   }

   put(Vessel("1", "Titanic"))
   put(Vessel("2", "Norge"))
   put(Vessel("3", "Eidsvold"))

}
