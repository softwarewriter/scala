package no.jergan.scrapbook

import cats.data.Kleisli

/**
 * What does this class do?
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
object PartialExample extends  App {

  def m(f: PartialFunction[String, Int]): Unit = ???

  m{
    case "a" => 42
    case "b" => 43
  }

  val v: PartialFunction[String, Int] = (a: String) => a match {
    case "a" => 42
    case "b" => 43
  }

  m(v)

  object WrappedFunction {

    case class WF[A, B](run: PartialFunction[A, B])

    def use: Unit = {

       val wf1 = WF[String, Int] { s => s.length}
       val wf2 = WF[String, Int] {
         val pf1: PartialFunction[String, Int] = {
           case null => 42
         }
         val pf2: PartialFunction[String, Int] = a => a.length
         pf1 //orElse pf2
       }

      println(wf1.run.getClass)
      println(wf2.run.getClass)
      println(wf2.run.isDefinedAt("hei"))
      println(wf2.run.apply("hei"))
    }
  }

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

  WrappedFunction.use

}

// scala format
