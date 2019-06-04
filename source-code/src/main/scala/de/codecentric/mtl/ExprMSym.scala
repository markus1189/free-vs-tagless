package de.codecentric.mtl

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.annotation.tailrec

//snippet:final-mtl-expr
trait ExprMSym[F[_]] {
  def intLit(value: Int): F[Int]
  def add(e1: Int, e2: Int): F[Int]

  def strLit(value: String): F[String]
  def concat(e1: String, e2: String): F[String]

  def strToInt(e: String): F[Int]
}
//snippet:end

object ExprMSym {
  implicit val interpMonad: Monad[Interp] = new Monad[Interp] {
    override def pure[A](x: A): Interp[A] = Interp(x)

    override def flatMap[A, B](fa: Interp[A])(f: A => Interp[B]): Interp[B] =
      f(fa.value)

    @tailrec override def tailRecM[A, B](a: A)(
        f: A => Interp[Either[A, B]]): Interp[B] = f(a).value match {
      case Left(a2)     => tailRecM(a2)(f)
      case Right(value) => Interp(value)
    }
  }

  //snippet:final-mtl-interp
  case class Interp[A](value: A) extends AnyVal

  implicit val exprSymInterp: ExprMSym[Interp] = new ExprMSym[Interp] {
    override def intLit(value: Int): Interp[Int] = Interp(value)

    override def add(e1: Int, e2: Int): Interp[Int] = Interp(e1 + e2)

    override def strLit(value: String): Interp[String] = Interp(value)

    override def concat(e1: String, e2: String): Interp[String] =
      Interp(e1 + e2)

    override def strToInt(e: String): Interp[Int] = Interp(e.toInt)
  }
  //snippet:end

  //snippet:final-mtl-sample
  def sampleProgram[F[_]: Monad](implicit expr: ExprMSym[F]): F[Int] = {
    import expr._

    for {
      four <- strLit("4")
      two <- strLit("2")
      concatenated <- concat(four, two)
      result <- strToInt(concatenated)
    } yield result
  }
  //snippet:end

  case class Print[A](value: String)

  implicit val expSymPrint: ExprMSym[Print] = new ExprMSym[Print] {
    override def intLit(value: Int): Print[Int] = Print(s"Int($value)")

    override def add(e1: Int, e2: Int): Print[Int] = Print(s"($e1 + $e2)")

    override def strLit(value: String): Print[String] = Print(s"String($value)")

    override def concat(e1: String, e2: String): Print[String] =
      Print(s"($e1 + $e2)")

    override def strToInt(e: String): Print[Int] = Print(s"strtoint($e)")
  }
}
