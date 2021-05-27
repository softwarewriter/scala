package no.jergan.scrapbook

object ProductWithSerializable {


  sealed trait T1 extends Product with Serializable

  case object CO11 extends T1
  case object CO12 extends T1
  case object CO13 extends T1
  case class CO14() extends T1

  sealed abstract class T2 extends Product with Serializable

  case object CO21 extends T2
  case object CO22 extends T2
  case object CO23 extends T2
  case class CO24() extends T2

  // Hm, the inferred types seems to work perfectly, not as described here:
  // https://typelevel.org/blog/2018/05/09/product-with-serializable.html
  val allT1 = Set(CO11, CO12)
  val allT2 = Set(CO21, CO22)

  // However, to get these to work, I need to extend Product with Serializable
  val allT11: Set[T1] = allT1
  val allT21: Set[T2] = allT2

}
