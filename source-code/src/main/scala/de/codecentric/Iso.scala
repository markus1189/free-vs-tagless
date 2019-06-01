package de.codecentric

import de.codecentric.`final`.ExprSym
import de.codecentric.initial.tagless._

object Iso {
  //snippet:final-to-initial
  def finalToInitial[F[_]: ExprSym, A](p: F[A]): ExprSym[Expr] =
    new ExprSym[Expr] {
      override def intLit(value: Int): Expr[Int] = IntLit(value)
      override def add(e1: Expr[Int], e2: Expr[Int]): Expr[Int] = Add(e1, e2)
      override def strLit(value: String): Expr[String] = StrLit(value)
      override def concat(e1: Expr[String], e2: Expr[String]): Expr[String] =
        Concat(e1, e2)
      override def strToInt(e: Expr[String]): Expr[Int] = StrToInt(e)
    }
  //snippet:end

  //snippet:initial-to-final
  def initialToFinal[F[_], A](p: Expr[A])(implicit interp: ExprSym[F]): F[A] =
    p match {
      case IntLit(value)  => interp.intLit(value)
      case Add(e1, e2)    => interp.add(initialToFinal(e1), initialToFinal(e2))
      case StrLit(value)  => interp.strLit(value)
      case Concat(e1, e2) => interp.concat(initialToFinal(e1), initialToFinal(e2))
      case StrToInt(e)    => interp.strToInt(initialToFinal(e))
    }
  //snippet:end
}
