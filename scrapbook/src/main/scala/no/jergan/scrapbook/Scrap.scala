package no.jergan.scrapbook

import cats.implicits.toBifunctorOps

import java.time.{LocalDate, OffsetDateTime}
import java.time.format.DateTimeFormatter
import scala.util.Try

/**
  * What does this class do?
  *
  * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
  */
object Scrap {

  case class A(a: String)

  def main(args: Array[String]): Unit = {
    val in: String = "2020-01-01"
    println(DateTimeFormatter.ISO_DATE.toString)
    println(in)
    println(Try(LocalDate.parse(in, DateTimeFormatter.ISO_DATE)).toEither.leftMap(_.getMessage))

  }

}
