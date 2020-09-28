package vessel

import cats.effect.{ConcurrentEffect, IO, Resource, Sync, Timer}
import org.http4s.{HttpRoutes, Response, Status}
import org.http4s.dsl.io.{->, /, GET, NotFound, Ok, Root}
import org.http4s.server.{Router, Server}

import scala.concurrent.ExecutionContext

/**
 * HTTP endpoints definitions.
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object Endpoints {

   def create[F[_] : ConcurrentEffect : Timer](configuration: Configuration, executionContext: ExecutionContext): Resource[F, Server[F]] = {

      val simpleHttpService: HttpRoutes[F] = HttpRoutes.of[F] {
         case GET -> Root / "simple" => Sync[F].pure {
            Response[F](Status.Ok).withEntity("hei")
         }
      }
     /*
      case Method.GET -> Mnt =>
        Sync[F].pure {
          Response[F](Status.Ok).withEntity(
            Json.obj(
              "version"    -> Json.fromString(version.getOrElse("NOT AN TAGGED VERSION")),
              "versionUrl" -> Json.fromString(versionUrl.getOrElse("NOT AN TAGGED VERSION"))
            ))
        }
    }

      */

      val routes = Router(
         ("simple", simpleHttpService)
      )

      platform.HttpServer[F](configuration.port, configuration.bindAddress, executionContext, routes)
   }

}