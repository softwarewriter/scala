package vessel

import scala.collection.mutable._

/**
 * Definition of vessel service
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
trait VesselService {

   def get(imo: String): Option[Vessel]

   def put(vessel: Vessel): Vessel

   def delete(imo: String): Option[Vessel]

   def search(query: String): List[Vessel]
}

object SimpleVesselService extends VesselService {

   private val storage: Map[String, Vessel] = new HashMap[String, Vessel]()

   override def get(imo: String): Option[Vessel] = {
      storage.get(imo)
   }

   override def put(vessel: Vessel): Vessel = {
      storage.put(vessel.imo, vessel)
      vessel
   }

   override def delete(imo: String): Option[Vessel] = {
      storage.remove(imo)
   }

   override def search(query: String): List[Vessel] = {
      storage.values.filter(vessel => vessel.imo.contains(query) || vessel.name.contains(query)).toList.sortBy(vessel => vessel.imo)
   }

   put(Vessel("1", "Titanic"))
   put(Vessel("2", "Norge"))
   put(Vessel("3", "Eidsvold"))

}
