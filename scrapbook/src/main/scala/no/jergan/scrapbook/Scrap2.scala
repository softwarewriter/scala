package no.jergan.scrapbook

import org.http4s.{ParseResult, Uri}

object Scrap2 {

  def main(args: Array[String]): Unit = {
    val l = List("ole", "dole", "doff")
    println(l.foldLeft(0)((size: Int, s: String) => size + s.length))
    println(l.foldRight(0)((s: String, size: Int) => size + s.length))

    val uri1 = Uri.unsafeFromString("klaveness-test-klab-c6f4.aivencloud.com:19526")

    val uri2 = Uri.fromString("172.17.0.1:34887").getOrElse(Uri.unsafeFromString("some-unparsable-kafka-uri"))
    println(uri1)
    println(uri2)
//    l.fold(0)(π(a1: Int, a2: Int) => a1 + a2)
  }

  /*
    /*
    def fold[B](z: B)(op: (B, Node) => B): B = {
      case RootNode(children)              => children
      case InternalNode(_, _, _, children) => children
      case ExternalNode              => op(z, Set.empty[Child]


    }

     */

  }


   */

}
