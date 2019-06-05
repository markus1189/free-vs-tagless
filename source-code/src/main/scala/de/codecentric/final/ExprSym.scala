package de.codecentric.`final`

//snippet:final-tagless-expr
trait ExprSym[Expr[_]] {
  def intLit(value: Int): Expr[Int]
  def add(e1: Expr[Int], e2: Expr[Int]): Expr[Int]

  def strLit(value: String): Expr[String]
  def concat(e1: Expr[String], e2: Expr[String]): Expr[String]

  def strToInt(e: Expr[String]): Expr[Int]
}
//snippet:end

object ExprSym {
  //snippet:final-tagless-interp
  case class Interp[A](value: A) extends AnyVal

  implicit val exprSymInterp: ExprSym[Interp] = new ExprSym[Interp] {
    override def intLit(value: Int): Interp[Int] = Interp(value)

    override def add(e1: Interp[Int], e2: Interp[Int]): Interp[Int] =
      Interp(e1.value + e2.value)

    override def strLit(value: String): Interp[String] = Interp(value)

    override def concat(e1: Interp[String],
                        e2: Interp[String]): Interp[String] =
      Interp(e1.value + e2.value)

    override def strToInt(e: Interp[String]): Interp[Int] =
      Interp(e.value.toInt)
  }
  //snippet:end

  //snippet:final-tagless-sample
  def sampleProgram[F[_]](implicit expr: ExprSym[F]): F[Int] = {
    import expr._
    strToInt(concat(strLit("4"), strLit("2")))
  }
  //snippet:end

  //snippet:final-tagless-pretty-print
  case class PP[A](value: String)

  implicit val expSymPrint: ExprSym[PP] = new ExprSym[PP] {
    override def intLit(value: Int): PP[Int] = PP(s"Int($value)")

    override def add(e1: PP[Int], e2: PP[Int]): PP[Int] =
      PP(s"(${e1.value} + ${e2.value})")

    override def strLit(value: String): PP[String] = PP(s"Str($value)")

    override def concat(e1: PP[String], e2: PP[String]): PP[String] =
      PP(s"(${e1.value} + ${e2.value})")

    override def strToInt(e: PP[String]): PP[Int] =
      PP(s"str2int(${e.value})")
  }
  //snippet:end

  {
    //snippet:final-tagless-pretty-print-example
    def sampleProgram[F[_]](implicit expr: ExprSym[F]): F[Int] = {
      import expr._
      strToInt(concat(strLit("4"), strLit("2")))
    }

    sampleProgram[PP].value // => str2int((Str(4) + Str(2)))
    //snippet:end
  }
}
