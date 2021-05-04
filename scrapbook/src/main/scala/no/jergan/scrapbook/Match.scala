package no.jergan.scrapbook

object Match {
  val a = "a"
  val b = "b"
  val A = "a"
  val B = "b"

  def m1(s: String) = s match {
    case "a" => "m1 a"
    case "b" => "m1 b"
  }

  def m2(s: String) = s match {
    case a => "m2 a"
    case b => "m2 b"
  }

  def m3(s: String) = s match {
    case `a` => "m3 a"
    case `b` => "m3 b"
  }

  def m4(s: String) = s match {
    case A => "m4 a"
    case B => "m4 b"
  }

  def main(args: Array[String]): Unit = {
    println(m1(a))
    println(m1(b))
    println(m2(a))
    println(m2(b))
    println(m3(a))
    println(m3(b))
    println(m4(a))
    println(m4(b))
  }

}
