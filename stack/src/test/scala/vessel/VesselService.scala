package vessel

import scala.collection.mutable._

/**
 * Definition of vessel service
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
trait VesselService {

   def get(imo: String): Option[Vessel]

   def put(vessel: Vessel): Unit

}

object SimpleVesselService extends VesselService {

   private val storage: Map[String, Vessel] = new HashMap[String, Vessel]()

   override def get(imo: String): Option[Vessel] = {
      storage.get(imo)
   }

   override def put(vessel: Vessel): Unit = {
      storage.put(vessel.imo, vessel)
   }

   put(Vessel("1", "Titanic"))
   put(Vessel("2", "Norge"))
   put(Vessel("3", "Eidsvold"))

}
