package no.jergan.scrapbook

object Exception {


  def main(args: Array[String]): Unit = {
    println("pelle1")
    try {
      Thread.currentThread().wait()
    }
    catch {
      case e: Exception => println(s"ex: $e")
    }
    println("pelle2")
  }
}
