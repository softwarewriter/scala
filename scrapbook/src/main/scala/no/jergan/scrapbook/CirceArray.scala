package no.jergan.scrapbook

import io.circe.Decoder
import io.circe.generic.auto._

import java.nio.file.{Files, Paths}
import scala.collection.mutable

object CirceArray {
// https://stackoverflow.com/questions/50381607/wildcard-path-in-circes-json-traversal

  class CountMap {

    val map: mutable.Map[String, Set[String]] = new mutable.HashMap[String, Set[String]]()

    def add(key: String, value: String): Unit = {
      map.get(key).fold[Unit](map.put(key, Set(value)))(set => map.put(key, set + value))
    }

    def asString(): String = {
      map
        .keys
        .toList
        .sorted
        .map(k => s"$k: ${map.get(k).map(s => s.size).getOrElse(0)}")
        .appended(s"Total: ${map.values.map(_.size).sum}\n")
        .mkString("\n")
    }

  }
  case class Shipment(id: String, cargoLines: List[CargoLine])
  case class CargoLine(
                        cargoLineId: String,
                        material: String
                      )

  case class Inventory(terminalId: String, materialName: String)

  val shipmentDecoder: Decoder[List[Shipment]] = Decoder[List[Shipment]].prepare(_.downField("shipments"))
  val inventoryDecoder: Decoder[List[Inventory]] = Decoder[List[Inventory]].prepare(_.downField("inventories"))

  def shipmentMap(prefix: String): String = {
    val json = Files.readString(Paths.get(s"/Users/oyvind/development/klaveness/temp/cargo-api-alcoa/prod/$prefix/shipments.json"))
 //   println(json)

    val cm = new CountMap()
    val shipments = io.circe.parser.decode(json)(shipmentDecoder)

//    println(shipments.map(ss => ss.map(s => s.cargoLines.length)))
//    println(shipments)

//    val cargoLines = io.circe.parser.decode(json)(cargoLineDecoder)
//    println(cargoLines)
    shipments.map(ss => ss.foreach(s => s.cargoLines.foreach(cl => cm.add(cl.material, s.id))))
    val result = cm.asString()
    Files.writeString(Paths.get(s"/Users/oyvind/development/klaveness/temp/cargo-api-alcoa/prod/$prefix/shipments.txt"), result)
    result
  }

  def inventoryMap(prefix: String): String = {
    val json = Files.readString(Paths.get(s"/Users/oyvind/development/klaveness/temp/cargo-api-alcoa/prod/$prefix/inventories.json"))
    //   println(json)

    val cm = new CountMap()
    val inventories = io.circe.parser.decode(json)(inventoryDecoder)

    //    println(shipments.map(ss => ss.map(s => s.cargoLines.length)))
    //    println(shipments)

    //    val cargoLines = io.circe.parser.decode(json)(cargoLineDecoder)
    //    println(cargoLines)
    inventories.map(is => is.foreach(i => cm.add(i.materialName, i.terminalId)))
    val result = cm.asString()
    Files.writeString(Paths.get(s"/Users/oyvind/development/klaveness/temp/cargo-api-alcoa/prod/$prefix/inventories.txt"), result)
    result
  }

  def main(args: Array[String]): Unit = {
    println(shipmentMap("before"))
    println(inventoryMap("before"))
  }

}
