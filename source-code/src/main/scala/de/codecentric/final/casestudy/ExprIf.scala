package de.codecentric.`final`.casestudy

import de.codecentric.`final`.ExprSym
import de.codecentric.`final`.ExprSym.Interp

//snippet:final-tagless-expr-if
trait ExprIf[Expr[_]] {
  def boolLit(value: Boolean): Expr[Boolean]
  def intToBool(e: Expr[Int]): Expr[Boolean]
  def ifExpr[A](c: Expr[Boolean])(ifTrue: () => Expr[A])(
      ifFalse: () => Expr[A]): Expr[A]
}
//snippet:end

object ExprSym {
  //snippet:final-tagless-interp-if
  // NO Interp class! Re-use it!

  implicit val exprIfInterp: ExprIf[Interp] = new ExprIf[Interp] {
    override def boolLit(value: Boolean): Interp[Boolean] = Interp(value)

    override def intToBool(e: Interp[Int]): Interp[Boolean] =
      Interp(e.value == 0)

    override def ifExpr[A](c: Interp[Boolean])(ifTrue: () => Interp[A])(
        ifFalse: () => Interp[A]): Interp[A] =
      if (c.value) ifTrue() else ifFalse()
  }
  //snippet:end

  //snippet:final-tagless-sample-if
  def sampleProgram[F[_]](implicit exprSym: ExprSym[F],
                          exprIf: ExprIf[F]): F[String] = {
    import exprIf._
    import exprSym._

    val condition: F[Boolean] = intToBool(strToInt(strLit("42")))
    ifExpr(condition)(() => strLit("it was true"))(() => strLit("it was false"))
  }
  //snippet:end

  def runProgram: String = sampleProgram[Interp].value
}
