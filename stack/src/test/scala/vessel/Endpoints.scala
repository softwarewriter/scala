package vessel

import cats.effect.{ConcurrentEffect, Resource, Timer}
import org.http4s.server.{Router, Server}

import scala.concurrent.ExecutionContext

/**
 * HTTP endpoints definitions.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object Endpoints {

   def create[F[_] : ConcurrentEffect : Timer](configuration: Configuration, executionContext: ExecutionContext): Resource[F, Server[F]] = {

      val routes = Router(
      )
      platform.HttpServer[F](configuration.port, configuration.bindAddress, executionContext, routes)
   }

}