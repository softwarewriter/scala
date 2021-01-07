package no.jergan.scrapbook

import no.jergan.pellemacro.PelleMacro

object UseMacro {


  def main(args: Array[String]): Unit = {

    val v1 = PelleMacro.pelle("hei")
    val v2 = PelleMacro.pelle(42)
    println("macro returned " + 1)
  }
}
