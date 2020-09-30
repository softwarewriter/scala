import io.unsecurity.Unsecurity
import io.unsecurity.auth.auth0.m2m.OauthAuthenticatedApplication

/**
 * What does this class do?
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
package object vessel {

   type ApplicationSecurity[F[_]] = Unsecurity[F, OauthAuthenticatedApplication, OauthAuthenticatedApplication]

   //  implicit val vesselEncoder = io.circe.generic.semiauto.deriveEncoder[Vessel]
   //   implicit val vesselDecoder = io.circe.generic.semiauto.deriveDecoder[Vessel]

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
