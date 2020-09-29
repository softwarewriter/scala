import io.unsecurity.Unsecurity
import io.unsecurity.auth.auth0.m2m.OauthAuthenticatedApplication

/**
 * Provides package wide imports and definitions.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
package object vessel {

   type ApplicationSecurity[F[_]] = Unsecurity[F, OauthAuthenticatedApplication, OauthAuthenticatedApplication]

}
