package de.codecentric.initial.tagless.optimization

import de.codecentric.initial.tagless.{Add, Expr, IntLit}

import scala.annotation.tailrec

class Optimizer {
  //snippet:optimizer-inline-addition
  def inlineAddition1[A](program: Expr[A]): Expr[A] = program match {
    case Add(IntLit(lhs), IntLit(rhs)) => IntLit(lhs + rhs)
    case Add(lhs, rhs)                 => Add(inlineAddition1(lhs), inlineAddition1(rhs))
    case _                             => program // why can we cheat here?
  }

  def inlineAddition[A](program: Expr[A]): Expr[A] = fixpoint[Expr[A]](program)(inlineAddition1)
  //snippet:end

  @tailrec private[this] def fixpoint[A](x: A)(f: A => A): A = {
    val x0 = x
    val x1 = f(x)
    if (x0 == x1) x0 else fixpoint(x1)(f)
  }
}
