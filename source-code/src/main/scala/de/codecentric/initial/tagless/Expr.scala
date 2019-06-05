package de.codecentric.initial.tagless

//snippet:initial-tagless-expr
sealed abstract class Expr[A] extends Product with Serializable
final case class IntLit(value: Int) extends Expr[Int]
final case class Add(e1: Expr[Int], e2: Expr[Int]) extends Expr[Int]
final case class StrLit(value: String) extends Expr[String]
final case class Concat(e1: Expr[String], e2: Expr[String]) extends Expr[String]
final case class StrToInt(e: Expr[String]) extends Expr[Int]
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

  //snippet:initial-tagless-pretty-print
  def prettyPrint[A](e: Expr[A]): String = e match {
    case IntLit(value) => s"Int($value)"
    case Add(e1, e2) => s"${prettyPrint(e1)} + ${prettyPrint(e2)}"
    case StrLit(value) => s"Str($value)"
    case Concat(e1, e2) => s"${prettyPrint(e1)} + ${prettyPrint(e2)}"
    case StrToInt(e) => s"str2int(${prettyPrint(e)})"
  }
  //snippet:end

  {
  //snippet:initial-tagless-pretty-print-example
    def sampleProgram: Expr[Int] = StrToInt(Concat(StrLit("4"), StrLit("2")))
    prettyPrint(sampleProgram) // => "str2int(Str(4) + Str(2))"
  //snippet:end
  }
}
