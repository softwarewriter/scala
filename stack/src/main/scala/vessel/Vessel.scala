package vessel

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}

/**
 * Representation of vessel.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
case class IMO(value: String) extends AnyVal

case class Vessel (imo: IMO, name: String)

object Vessel {

//  implicit val vesselEncoder = io.circe.generic.semiauto.deriveEncoder[Vessel]
//   implicit val vesselDecoder = io.circe.generic.semiauto.deriveDecoder[Vessel]

  implicit val imoCodec = io.circe.generic.semiauto.deriveCodec[IMO]
  implicit val vesselCodec = io.circe.generic.semiauto.deriveCodec[Vessel]

  /*
   implicit val vesselEncoder: Encoder[Vessel] = (vessel: Vessel) => {
      Json
         .obj(
            "imo" := vessel.imo,
            "name" := vessel.name
         )
         .dropNullValues
   }

   implicit val decodeUser: Decoder[Vessel] =
      Decoder.forProduct2("imo", "name")(Vessel.apply)

   */

}
