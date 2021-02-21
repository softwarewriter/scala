package no.jergan.scrapbook

import io.circe
import io.circe.Decoder.Result
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json, Parser, parser}
import io.circe.parser._

import scala.Console.println

object CirceTest {

  case class Thing(foo: String, bars: List[String])
  case class Bar(name: String)

  implicit val decodeFoo: Decoder[Thing] = new Decoder[Thing] {
    final def apply(c: HCursor): Decoder.Result[Thing] = {
      println(c)
//      println(c.keys)

    for {
      foo <- c.get[String]("foo")
//        foo <- c.downField("foo").as[String]
        bar <- c.downField("bars").downArray.as[String]
      } yield {
        new Thing(foo, List(bar))
      }
    }
  }

  /*
  implicit val decodeBar: Decoder[Bar] = new Decoder[Bar] {
    final def apply(c: HCursor): Decoder.Result[Bar] = {
      for {
        name <- c.downField("name").downArray.as[String]
      } yield {
        new Bar(name)
      }
    }
  }

   */

  def main(args: Array[String]): Unit = {

//    val result: Json = Thing("ole", 42)
    val s = """{ "foo": "ole", "bars": [ "n1", "n2" ] }"""
//    val s = """{ "foo": "ole", "bars": [ { "n11", "n12" ] }, { ["n21", "n22" ] } ] }"""
    println(s)
    val result: Either[circe.Error, Thing] = parser.decode[Thing](s)
    println(result)

//    println(result)
  }

  // single array:
  // all downArray and downArray.first and downArray.first.first works
  // downArray.downArray does not work

}
