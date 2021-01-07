package no.jergan.`macro`

import language.experimental.macros
import scala.reflect.macros.blackbox.Context

object PelleMacro {

  def hello(): Unit = macro hello_impl

  def hello_impl(c: Context)(): c.Expr[Unit] = {
    import c.universe._
    reify { println("Hello World!") }
  }

}
