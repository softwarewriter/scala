package no.jergan.pellemacro

import language.experimental.macros
import scala.reflect.macros.blackbox.Context

object PelleMacro {

  def pelle(message: Any): Int = macro pelle_impl

  def pelle_impl(c: Context)(message: c.Expr[Any]): c.Expr[Int] = {
    import c.universe._
    val paramRep: String = show(message.tree)
    val paramRepTree: c.universe.Literal = Literal(Constant(paramRep))
    val paramRepExpr: c.Expr[String] = c.Expr[String](paramRepTree)

    val v: c.universe.Expr[Int] = reify {
      println(paramRepExpr.splice + " contains " + message.splice)
      42
    }
    v
  }

}
