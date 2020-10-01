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


}
