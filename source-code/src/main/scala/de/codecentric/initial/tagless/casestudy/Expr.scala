package de.codecentric.initial.tagless.casestudy

import de.codecentric.initial.tagless._

//snippet:initial-tagless-expr-if
sealed abstract class Expr[A] extends Product with Serializable
final case class IntLit(value: Int) extends Expr[Int]
final case class Add(e1: Expr[Int], e2: Expr[Int]) extends Expr[Int]
final case class StrLit(value: String) extends Expr[String]
final case class Concat(e1: Expr[String], e2: Expr[String]) extends Expr[String]
final case class StrToInt(e: Expr[String]) extends Expr[Int]

final case class BoolLit(value: Boolean) extends Expr[Boolean]
final case class If[A](condition: Expr[Boolean],
                       ifTrue: () => Expr[A],
                       ifFalse: () => Expr[A])
    extends Expr[A]
//snippet:end

object Interpreter {
  //snippet:initial-tagless-sample-if
  def sampleProgram: Expr[Int] =
    If(BoolLit(true), { () =>
      IntLit(42)
    }, { () =>
      IntLit(21)
    })
//snippet:end

  //snippet:initial-tagless-interp-if
  def interp[A](e: Expr[A]): A = e match {
    case IntLit(value)  => value
    case Add(e1, e2)    => handleAdd(e1, e2)
    case StrLit(value)  => value
    case Concat(e1, e2) => handleConcat(e1, e2)
    case StrToInt(e_)   => handleStrToInt(e_)
    case BoolLit(value) => value
    case If(c, t, f)    => handleIf(c, t, f)
  }
  //snippet:end

  //snippet:initial-tagless-handle-if
  private[this] def handleIf[A](condition: Expr[Boolean],
                                ifTrue: () => Expr[A],
                                ifFalse: () => Expr[A]): A =
    if (interp(condition)) interp(ifTrue()) else interp(ifFalse())
  //snippet:end

  private[this] def handleAdd(e1: Expr[Int], e2: Expr[Int]): Int =
    interp(e1) + interp(e2)

  private[this] def handleConcat(e1: Expr[String], e2: Expr[String]): String =
    interp(e1) + interp(e2)

  private[this] def handleStrToInt(e: Expr[String]) =
    interp(e).toInt
}
