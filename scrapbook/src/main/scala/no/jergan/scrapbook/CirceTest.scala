package no.jergan.scrapbook

import io.circe
import io.circe.Decoder.Result
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json, Parser, parser}
import io.circe.parser._

import scala.Console.println

object CirceTest {

  case class Thing(foo: String, bars: List[Bar])
  case class Bar(name: String)

  /*
  implicit val encodeFoo: Encoder[Thing] = new Encoder[Thing] {
    def apply(a: Thing): Json = Json.obj(
      ("foo", Json.fromString(a.foo)),
      ("bars", Json.fromValues(a.bars))
    )
  }

   */

  implicit val decodeFoo: Decoder[Thing] = new Decoder[Thing] {
    final def apply(c: HCursor): Decoder.Result[Thing] = {
      println(c)
//      println(c.keys)

    for {
        foo <- c.downField("foo").as[String]
        bars <- c.downField("bars").as[List[Bar]]
      } yield {
        new Thing(foo, bars)
      }
    }
  }

  implicit val decodeBar: Decoder[Bar] = new Decoder[Bar] {
    final def apply(c: HCursor): Decoder.Result[Bar] = {
      for {
        name <- c.downField("name").as[String]
      } yield {
        new Bar(name)
      }
    }
  }

  def main(args: Array[String]): Unit = {

//    val result: Json = Thing("ole", 42)
    val s = """{ "foo": "ole", "bars": [ {"name": "hei"}, {"name": "nei"} ] }"""
    println(s)
    val result: Either[circe.Error, Thing] = parser.decode[Thing](s)
    println(result)

//    println(result)
  }

}
