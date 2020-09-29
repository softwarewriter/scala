package vessel

import io.circe.Decoder.Result
import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.syntax._

/**
 * Representation of vessel.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
case class Vessel (imo: String, name: String)

object Vessel {

   implicit val vesselEncoder: Encoder[Vessel] = (vessel: Vessel) => {
      Json
         .obj(
            "IMO" := vessel.imo,
            "Name" := vessel.name
         )
         .dropNullValues
   }

   implicit val decodeUser: Decoder[Vessel] =
      Decoder.forProduct2("IMO", "Name")(Vessel.apply)

}
