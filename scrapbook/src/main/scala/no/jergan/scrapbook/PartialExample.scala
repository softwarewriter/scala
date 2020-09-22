package no.jergan.scrapbook

/**
 * What does this class do?
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object PartialExample extends  App {

   def completeFunction(i:Int)(x: Int): Int = i * x

   //total or full function, e.g. pure and referential transparent  val doublerFunction: Int => Int = completeFunction(2)
   // partial applied function  val six = doublerFunction(3) //6

   def oddsPrinter:  PartialFunction[Int, Unit] = { // partial function
      case d: Int if d % 2 == 1 && d >=0 => println(s"$d is an odd number")
   }

   def evensPrinter:  PartialFunction[Int, Unit] = { // partial function
      case d: Int if d % 2 == 0 && d >=0 => println(s"$d is an even number")
   }

   def theRest:  PartialFunction[Int, Unit] = {
      case _:Int => println("None of the above")
   }

   def completeNumberFunction: PartialFunction[Int, Unit] = evensPrinter orElse oddsPrinter orElse theRest

   println(evensPrinter.isDefinedAt(-2))
   println(evensPrinter.isDefinedAt(1))

   oddsPrinter.apply(1)
   evensPrinter.apply(2)
   completeNumberFunction.apply(1)
   completeNumberFunction.apply(2)
   completeNumberFunction.apply(-2)

}
