package no.jergan.scrapbook

import cats.Applicative
import cats.implicits.{toBifunctorOps, toTraverseOps}
import cats.instances.vector._

import java.time.{LocalDate, OffsetDateTime}
import java.time.format.DateTimeFormatter
import scala.util.Try


/**
  * What does this class do?
  *
  * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
  */
object Scrap {

  def main(args: Array[String]): Unit = {
/*
    def pf1: PartialFunction[Int, String] = {
      case 1 => "en"
      case 2 => "to"
    }
    def pf2: PartialFunction[Int, String] = {
      case 3 => "tre"
      case 4 => "fire"
    }

    val pf = pf1.orElse(pf2)

    println(pf(5))

 */

    val l = List(1, 2, 3, 4, 1, 2, 5)
    println(l.distinct)
  }

}
