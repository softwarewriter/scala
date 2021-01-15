package no.jergan.scrapbook

import io.circe.{Encoder, Json}
import io.circe.syntax._
import io.circe.parser._

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import scala.io.Source

object Memory {

  case class Person(name: String, age: Int)

  val filename: String = "/Users/oyvind/tmp/personsRaw.json"

  implicit val personsEncoder: Encoder[List[Person]] = (persons: List[Person]) => {
    Json.arr(persons.map(p => personEncoder(p)):_*)
    /*
    Json
      .obj(
        "persons" := Json.arr(persons.map(p => personEncoder(p)):_*)
      )
      .dropNullValues

     */
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
    val persons: List[Person] = (1 to n).toList.map(i => Person(s"name $i", i))
    persons.asJson
  }

  def countElements(string: String): Option[Int] = {
    parse(string)
      .toOption
      .flatMap(j => j.hcursor.values)
      .map(v => v.size)
  }

}
