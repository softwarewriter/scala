package no.jergan.scrapbook

import io.circe.Decoder.Result
import io.circe.{Decoder, Encoder, HCursor, Json, ParsingFailure}
import io.circe.syntax._
import io.circe.parser._

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import scala.io.Source

object Memory {

  case class Person(name: String, age: Int/*, books: List[Book]*/)
  case class Book(title: String, author: String, pages: Int)

  val filename: String = "/Users/oyvind/tmp/persons.json"

  implicit val personsEncoder: Encoder[List[Person]] = (persons: List[Person]) => {
    Json
      .obj(
        "persons" := Json.arr(persons.map(p => personEncoder(p)):_*)
      )
      .dropNullValues
  }

  implicit val personEncoder: Encoder[Person] = (person: Person) => {
    Json
      .obj(
        "name" := person.name,
        "age" := person.age
      )
      .dropNullValues
  }

  /*
  implicit val personsDecoder: Decoder[List[Person]] = (c: HCursor) => {
    for {
      ps <- c.downField("persons").as[List[Person]]
    } yield ps
  }

  implicit val personDecoder: Decoder[Person] = (c: HCursor) => {
    for {
      name      <- c.downField("name").as[String]
      age       <- c.downField("age").as[Int]
    } yield Person(name, age)
  }

   */

  def main(args: Array[String]): Unit = {
//    write()
    read()

    //    println(string)

//    println(countElements(string))

//    val parsed: Either[ParsingFailure, Json] = parse(string)
//    println(parsed)

//    val r = parsed.map(j => j.as[List[Person]])
//    val r = parsed.map(j => j.asJson(personsDecoder))
//    println(r)

//    println(r)//
  }

  def write(): Unit = {
    val json: Json = generate(1000 * 1000)
    val string = json.asJson.noSpaces
    Files.write(Paths.get(filename), string.getBytes(StandardCharsets.UTF_8))
  }

  def read(): Unit = {
    val string = Source.fromFile(filename).mkString
    println("string length: " + string.length)
    println(countElements(string)) // fails on -Xmx300m
  }

  def generate(n: Int): Json = {
//    val books = (1 to 5).toList.map(i => Book(s"title $i", s"author + $i", i * 100))
    val persons: List[Person] = (1 to n).toList.map(i => Person(s"name $i", i/*, books*/))
    persons.asJson
  }

  def countElements(string: String): Option[Int] = {
    /*
    val parsed: Either[ParsingFailure, Json] = parse(string)
    parsed match {
      case Left(_) => None
      case Right(json) => {
//        println(json)
        val c = json.hcursor.downField("persons")
        println(c)
        val v: Option[Iterable[Json]] = c.values
        v match {
          case Some(vec) => println(vec.size)
          case None =>
        }

//        println(json.hcursor.downField("persons"))
      }
    }

     */
    parse(string)
      .toOption
      .flatMap(j => j.hcursor.downField("persons").values)
      .map(v => v.size)
  }

}
