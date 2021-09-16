package no.jergan.scrapbook.scalawithcats

import cats.data.State
import cats.implicits.{toFlatMapOps, toFunctorOps}

object Ex493 {

  type CalcState[A] = State[List[Int], A]

  object CalcState {
    def apply[A](f: List[Int] => (List[Int], A)): CalcState[A] = State[List[Int], A](f)
  }

  def evalOne(symbol: String): CalcState[Int] =
    symbol match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }

  def evalAll(symbols: String): CalcState[Int] = {
    evalAll(symbols.split(" ").toList)
  }

  def evalAll(symbols: List[String]): CalcState[Int] = {
    symbols.foldLeft(CalcState(s => (s, 0)))((b, a) => b.flatMap(_ => evalOne(a)))
  }

  def operator(f: (Int, Int) => Int): CalcState[Int] = CalcState[Int] { stack => {
    val result = f(stack.head, stack.drop(1).head)
    (result :: stack.drop(2), result)
  }
  }

  def operand(num: Int): CalcState[Int] =
    CalcState[Int] { stack =>
      (num :: stack, num)
    }

  def main(args: Array[String]): Unit = {

    val v = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      res <- evalOne("+")
    } yield res
    println(v.run(Nil).value)
    println(evalAll("1 3 + 5 *").run(Nil).value)
  }

}
