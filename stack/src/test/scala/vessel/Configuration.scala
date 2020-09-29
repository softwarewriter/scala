package vessel

import platform.Database

/**
 * Configuration for application.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
case class Configuration(val port: Int, bindAddress: String, vesselService: VesselService, databaseConfiguration: Database.Config) {


}
