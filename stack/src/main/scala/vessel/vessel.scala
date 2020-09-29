import io.unsecurity.Unsecurity
import io.unsecurity.auth.auth0.m2m.OauthAuthenticatedApplication

/**
 * What does this class do?
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
package object vessel {

   type ApplicationSecurity[F[_]] = Unsecurity[F, OauthAuthenticatedApplication, OauthAuthenticatedApplication]

}
