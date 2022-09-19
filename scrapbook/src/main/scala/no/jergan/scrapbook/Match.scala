package no.jergan.scrapbook

object Match {

  class Person(val firstName: String, val lastName: String)

  object Person {
    def unapplySeq(p: Person): Option[List[String]] = Some(List(p.firstName, p.firstName, p.lastName))
  }

  class Numbers(val number1: Int, val number2: Int)

  object Numbers {
    def unapply(n: Numbers): Option[Int] = Some(n.number1 + n.number2)
  }

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

  def m5(): Unit = {
    val numbers: Numbers = new Numbers(1, 2)
    numbers match {
      case Numbers(3) => println("numbers 1 and 2")
    }
  }

  def m6(): Unit = {
    val request = new Person("Ole", "Hansen")

    request match {
      case Person(h1)       => println(s"The only name is $h1")
      case Person(h1, h2)   => println(s"We have two names: $h1 and $h2")
      case Person(all @ _*) => print(s"All names are as following: $all")
    }
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
    println(m5())
    println(m6())
  }

}
