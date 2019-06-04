package de.codecentric.`final`.optimization

import de.codecentric.`final`.ExprSym
import de.codecentric.`final`.ExprSym.{Interp, Print}

object Main extends App {
  def p[F[_]](implicit lang: ExprSym[F]) = {
    import lang._

    add(add(intLit(5), intLit(6)), strToInt(strLit("0")))
  }

  def p2[F[_]](implicit lang: ExprSym[F]) = {
    import lang._

    add(add(intLit(1), intLit(2)), add(intLit(3), intLit(4)))
  }

  type OptInterp[A] = Opt[Interp, A]

  println(p2[OptInterp].run(List()))

  type OptPrint[A] = Opt[Print, A]

  println(s"Pretty: ${p[Print].value}")
  println(s"OptPretty: ${p[OptPrint].run(List())._2.value}")


  println(p2(Optimizer2.optimizer2[Print]).first())
}