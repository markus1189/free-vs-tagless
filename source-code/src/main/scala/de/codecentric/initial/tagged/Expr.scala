package de.codecentric.initial.tagged

import cats.syntax.either._

sealed abstract class Expr extends Product with Serializable
final case class IntLit(value: Int) extends Expr
final case class Add(e1: Expr, e2: Expr) extends Expr
final case class StrLit(value: String) extends Expr
final case class Concat(e1: Expr, e2: Expr) extends Expr
final case class StrToInt(e: Expr) extends Expr

sealed abstract class Result
final case class IntResult(value: Int) extends Result
final case class StrResult(value: String) extends Result

object Interpreter {
  def sampleProgram = StrToInt(Concat(StrLit("4"), StrLit("2")))

  def problematic = StrToInt(IntLit(42))

  def interp(e: Expr): Either[String, Result] = e match {
    case IntLit(value)  => IntResult(value).asRight
    case Add(e1, e2)    => handleAdd(e1, e2)
    case StrLit(value)  => StrResult(value).asRight
    case Concat(e1, e2) => handleConcat(e1, e2)
    case StrToInt(e_)   => handleStrToInt(e_)
  }

  private[this] def handleAdd(e1: Expr, e2: Expr): Either[String, Result] =
    for {
      r1 <- interp(e1)
      r2 <- interp(e2)
      result <- (r1, r2) match {
        case (IntResult(v1), IntResult(v2)) => IntResult(v1 + v2).asRight
        case _                              => s"Could not add $r1 and $r2".asLeft
      }
    } yield result

  private[this] def handleConcat(e1: Expr, e2: Expr): Either[String, Result] =
    for {
      r1 <- interp(e1)
      r2 <- interp(e2)
      result <- (r1, r2) match {
        case (StrResult(v1), StrResult(v2)) => StrResult(v1 + v2).asRight
        case _                              => s"Could not concat $r1 and $r2".asLeft
      }
    } yield result

  private[this] def handleStrToInt(e: Expr): Either[String, IntResult] =
    for {
      r <- interp(e)
      result <- r match {
        case StrResult(value) => IntResult(value.toInt).asRight
        case IntResult(value) => s"invalid argument $r".asLeft
      }
    } yield result

}
