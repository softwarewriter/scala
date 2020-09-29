package vessel

import cats.effect.IO
import io.circe.{Encoder, Json}
import io.circe.syntax._

/**
 * Representation of vessel.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
case class Vessel (imo: String, name: String)

object Vessel {
   implicit val personEncoder: Encoder[Vessel] = (vessel: Vessel) => {
      Json
         .obj(
            "IMO" := vessel.imo,
            "Name" := vessel.name
         )
         .dropNullValues
   }

   implicit def entityEncoder[A: Encoder] = org.http4s.circe.jsonEncoderOf[IO, A]

}
