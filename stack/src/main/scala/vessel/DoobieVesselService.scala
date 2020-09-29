package vessel

/**
 * Implementation of [[VesselService]] that targets doobie.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class DoobieVesselService extends VesselService {

   override def get(imo: String): Option[Vessel] = ???

   override def put(vessel: Vessel): Vessel = ???

   override def delete(imo: String): Option[Vessel] = ???

   override def search(query: String): List[Vessel] = ???

}
