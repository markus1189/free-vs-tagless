package de.codecentric.initial.tagless

//snippet:initial-tagless-expr
sealed abstract class Expr[A] extends Product with Serializable
final case class IntLit(value: Int) extends Expr[Int]
final case class Add(e1: Expr[Int], e2: Expr[Int]) extends Expr[Int]
final case class StrLit(value: String) extends Expr[String]
final case class Concat(e1: Expr[String], e2: Expr[String]) extends Expr[String]
final case class StrToInt(e: Expr[String]) extends Expr[Int]
//snippet:end

//snippet:initial-tagless-ctors
//object Expr {
//  def intLit(value: Int): Expr[Int] = IntLit(value)
//  def add(e1: Expr[Int], e2: Expr[Int]) = Add(e1, e2)
//
//  def strLit(value: String): Expr[String] = StrLit(value)
//  def concat(e1: Expr[String], e2: Expr[String]) = Concat(e1, e2)
//
//  def strToInt(e: Expr[String]): Expr[Int] = StrToInt(e)
//}
//snippet:end

object Interpreter {
  //snippet:initial-tagless-sample
  def sampleProgram: Expr[Int] = StrToInt(Concat(StrLit("4"), StrLit("2")))

  // does no longer compile:
  // def problematic = StrToInt(IntLit(42))
  //snippet:end

  //snippet:initial-tagless-interp
  def interp[A](e: Expr[A]): A = e match {
    case IntLit(value)  => value
    case Add(e1, e2)    => handleAdd(e1, e2)
    case StrLit(value)  => value
    case Concat(e1, e2) => handleConcat(e1, e2)
    case StrToInt(e_)   => handleStrToInt(e_)
  }
  //snippet:end

  //snippet:initial-tagless-add
  private[this] def handleAdd(e1: Expr[Int], e2: Expr[Int]): Int =
    interp(e1) + interp(e2)
  //snippet:end

  private[this] def handleConcat(e1: Expr[String], e2: Expr[String]): String =
    interp(e1) + interp(e2)

  private[this] def handleStrToInt(e: Expr[String]) =
    interp(e).toInt
}
