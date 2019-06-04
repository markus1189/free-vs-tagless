package de.codecentric.free

import cats.free.Free
import cats.{Monad, ~>}

//snippet:initial-free-expr
sealed abstract class ExprF[A] extends Product with Serializable
final case class IntLit(value: Int) extends ExprF[Int]
final case class Add(e1: Int, e2: Int) extends ExprF[Int]
final case class StrLit(value: String) extends ExprF[String]
final case class Concat(e1: String, e2: String) extends ExprF[String]
final case class StrToInt(e: String) extends ExprF[Int]
//snippet:end

object ExprF {
  //snippet:initial-free-ctors
  type Expr[A] = Free[ExprF, A]

  def intLit(value: Int): Expr[Int] = Free.inject(IntLit(value))
  def add(e1: Int, e2: Int): Expr[Int] = Free.inject(Add(e1, e2))

  def strLit(value: String): Expr[String] = Free.inject(StrLit(value))
  def concat(e1: String, e2: String): Expr[String] = Free.inject(Concat(e1, e2))

  def strToInt(e: String): Expr[Int] = Free.inject(StrToInt(e))
  //snippet:end
}

object Interpreter {
  import ExprF._

  //snippet:initial-free-sample
  def sampleProgram: Expr[Int] =
    for {
      four <- strLit("4")
      two <- strLit("2")
      concatenated <- concat(four, two)
      result <- strToInt(concatenated)
    } yield result
  //snippet:end

  //snippet:initial-free-interp
  def interp[A, M[_]: Monad](expr: Expr[A]): M[A] =
    expr.foldMap(new (ExprF ~> M) {
      override def apply[X](fa: ExprF[X]): M[X] = fa match {
        case IntLit(value)  => Monad[M].pure(value)
        case Add(e1, e2)    => Monad[M].pure(e1 + e2)
        case StrLit(value)  => Monad[M].pure(value)
        case Concat(e1, e2) => Monad[M].pure(e1 + e2)
        case StrToInt(e)    => Monad[M].pure(e.toInt)
      }
    })
  //snippet:end
}
