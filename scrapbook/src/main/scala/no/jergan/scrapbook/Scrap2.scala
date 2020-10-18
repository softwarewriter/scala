package no.jergan.scrapbook

object Scrap2 {

  def main(args: Array[String]): Unit = {
    val l = List("ole", "dole", "doff")
    println(l.foldLeft(0)((size: Int, s: String) => size + s.length))
    println(l.foldRight(0)((s: String, size: Int) => size + s.length))

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
