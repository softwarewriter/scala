package no.jergan.scrapbook

/**
  * What does this class do?
  *
  * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
  */
object Scrap extends App {

  val list: IndexedSeq[Int] = for (i <- 0 until 20 if (i % 2 == 0)) yield i
  println(list.size)


  //   println(m(block))

  // functions
  def sq1: Int => Int = (x: Int) => x * x
  val sq2: Int => Int = (x: Int) => x * x

  // methods
  def sq3(x: Int): Int = x * x

  //   val sq4(x: Int): Int = x * x

  println(sq1)
  println(sq2)
  //   println(sq3)

  println(sq3(3))

  trait Greeter {
    def greet(name: String): Int
  }

  trait Greeter2 {
    def greet2: String => Int
  }

  class Impl extends Greeter with Greeter2 {

    override def greet(name: String): Int = 42

    override def greet2: String => Int = (s: String) => 43
    //     override def greet: String => Int = (s: String => 42
  }
  val any1: Any    = Int.MaxValue
  val any2: AnyVal = Int.MinValue
  val any3: AnyRef = "hei"
   val any4: AnyRef = (a: String) => a.length

  val anyList: List[Any] = List(any1, any2, any3, any4)
  println(anyList)

  val f: (Int, Int) => String = (a: Int, b: Int) => s"f: $a, $b"

  def m(a: Int = 0, b: Int) = s"m: $a, $b"

  println(f(1, 2))
  println(m(1, 2))
  println(m(b = 2))


  val tu: (Int, Int) = (2, 3)
  val tu2: Tuple2[Int, Int] = Tuple2(4, 5)
  val tu3: Tuple2[Int, Int] = 4 -> 5
  println(tu)
  println(tu.getClass)
  println(tu2.getClass)

  println(tu._1)
  println(tu3)

  val (a: Int, b) = tu
  println(a)
  2 match {
    case (1) => println(1)
    case (_) => println("røkla")
  }

  abstract class AbsIterator {
    type T
    def hasNext: Boolean
    def next(): T
  }

  class StringIterator(s: String) extends AbsIterator {
    type T = Char
    private var i = 0
    def hasNext = i < s.length
    def next() = {
      val ch = s charAt i
      i += 1
      ch
    }
  }

  def urlBuilder(ssl: Boolean, domainName: String): (String, String) => String = {
    val schema = if (ssl) "https://" else "http://"
    (endpoint: String, query: String) => s"$schema$domainName/$endpoint?$query"
  }

  val domainName = "www.example.com"
  def getURL = urlBuilder(ssl=true, domainName)
  val endpoint = "users"
  val query = "id=1"
  val url = getURL(endpoint, query) // "https://www.example.com/users?id=1": String

}
