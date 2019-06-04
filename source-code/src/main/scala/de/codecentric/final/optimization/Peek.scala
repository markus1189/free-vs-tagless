package de.codecentric.`final`.optimization

import de.codecentric.`final`.ExprSym

sealed abstract class Peek[+A]

object Peek {
  case class IntLit(value: Int) extends Peek[Nothing]
  case object Add extends Peek[Nothing]
  case class StrLit(value: String) extends Peek[Nothing]
  case object Concat extends Peek[Nothing]
  case object StrToInt extends Peek[Nothing]

  implicit val peekExprSym: ExprSym[Peek] = new ExprSym[Peek] {
    override def intLit(value: Int): Peek[Int] = Peek.IntLit(value)

    override def add(e1: Peek[Int], e2: Peek[Int]): Peek[Int] = Peek.Add

    override def strLit(value: String): Peek[String] = Peek.StrLit(value)

    override def concat(e1: Peek[String], e2: Peek[String]): Peek[String] =
      Peek.Concat

    override def strToInt(e: Peek[String]): Peek[Int] = Peek.StrToInt
  }
}
