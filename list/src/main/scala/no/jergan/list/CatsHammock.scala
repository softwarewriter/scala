package no.jergan.list

import cats.effect.IO
import hammock._
import hammock.marshalling._
import hammock.apache.ApacheInterpreter
import hammock.circe.implicits._

/**
 * What does this class do?
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object CatsHammock {

   def main(args: Array[String]): Unit = {



      object HttpClient {
         // Using the Apache HTTP commons interpreter
         implicit val interpreter = ApacheInterpreter.instance[IO]

         val response = Hammock
            .request(Method.GET, uri"https://api.fidesmo.com/apps", Map()) // In the `request` method, you describe your HTTP request
            .as[List[String]]
            .exec[IO]
      }
   }
}
