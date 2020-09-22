package no.jergan.scrapbook

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.{HttpApp, HttpRoutes, Method, Request, Response, Status, Uri}
import org.http4s.dsl.io._
import org.http4s.implicits._
import cats.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder

/**
 * What does this class do?
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object HTTPServer extends IOApp {

   /*
     HttpServer === En eller flere partial functions med signatur Request => Response
     val r1 = GET /hello
     val r2 = GET /hello2
     val r3 = GET /hello3
     val theRest => NotFound
     r1 orElse r2 orElse r3 orElse theRest     httpKall         == Request => Response
     effekt i tillegg == Request => F[Response]  == Kleisli[OptionT[F, *], Request, Response]

    */  val helloService: HttpRoutes[IO] = HttpRoutes.of[IO]{
      case Request(Method.GET,Uri(_, _, "/hello", _, _),_,_,_, _) => IO(Response[IO](Status.Ok))
   }

   val echoService: HttpRoutes[IO] = HttpRoutes.of[IO]{
      case GET -> Root / "echo" / what => Ok(s"you said [$what]")
   }

   def routes(rs: List[HttpRoutes[IO]]): HttpApp[IO]= {
      rs.reduce( _ <+> _ ).orNotFound
   }

   override def run(args: List[String]): IO[ExitCode] = {
      BlazeServerBuilder[IO](scala.concurrent.ExecutionContext.global)
         .bindHttp(1337)
         .withHttpApp(routes(List(helloService,echoService)))
         .serve
         .compile
         .drain
         .map(_ => ExitCode.Success)
   }
}

