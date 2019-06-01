package de.codecentric.`final`.optimization

import de.codecentric.`final`.ExprSym
import de.codecentric.`final`.ExprSym.{Interp, Print}
import de.codecentric.`final`.optimization.Ctx._

//snippet:final-tagless-opt-ctx
sealed abstract class Ctx
object Ctx {
  case class CtxInt(value: Int) extends Ctx
  case object CtxAdd extends Ctx
  case object CtxOther extends Ctx
}
//snippet:end

//snippet:final-tagless-opt-type
case class Opt[F[_], A](run: List[Ctx] => (List[Ctx], F[A]))
//snippet:end

object Snippet {
  //snippet:final-tagless-opt-sig
  // Using kind-projector for Lambda
  implicit def inlineAdditionExprSym[F[_]](
      implicit base: ExprSym[F]): ExprSym[Lambda[x => Opt[F, x]]] = ???
  //snippet:end
}

object Opt {
  implicit def inlineAdditionExprSym[F[_]](
      implicit base: ExprSym[F]): ExprSym[Lambda[x => Opt[F, x]]] = {
    new ExprSym[Lambda[x => Opt[F, x]]] {

      //snippet:final-tagless-opt-impl
      override def intLit(value: Int): Opt[F, Int] =
        Opt(ctx => (CtxInt(value) :: ctx, base.intLit(value)))

      override def add(e1: Opt[F, Int], e2: Opt[F, Int]): Opt[F, Int] = Opt {
        ctx0 =>
          val (ctx1, v1) = e1.run(CtxAdd :: ctx0)
          val (ctx2, v2) = e2.run(CtxAdd :: ctx1)

          ctx2 match {
            case CtxInt(lhs) +: CtxAdd +: CtxInt(rhs) +: CtxAdd +: ctxs =>
              (CtxInt(lhs + rhs) :: ctxs, base.intLit(lhs + rhs))
            case _ => (CtxAdd :: ctx2, base.add(v1, v2))
          }
      }
      //snippet:end

      override def strLit(value: String): Opt[F, String] =
        Opt(ctx => (CtxOther :: ctx, base.strLit(value)))

      override def concat(e1: Opt[F, String],
                          e2: Opt[F, String]): Opt[F, String] = Opt { ctx0 =>
        val (ctx1, v1) = e1.run(CtxOther :: ctx0)
        val (ctx2, v2) = e2.run(CtxOther :: ctx1)

        (CtxOther :: ctx2, base.concat(v1, v2))
      }

      override def strToInt(e: Opt[F, String]): Opt[F, Int] = Opt { ctx0 =>
        val (ctx1, v) = e.run(CtxOther :: ctx0)
        (CtxOther :: ctx1, base.strToInt(v))
      }
    }
  }
}

object Test extends App {
  def p[F[_]](implicit lang: ExprSym[F]) = {
    import lang._

    add(add(intLit(5), intLit(6)), strToInt(strLit("0")))
  }

  def p2[F[_]](implicit lang: ExprSym[F]) = {
    import lang._

    add(add(intLit(1), intLit(2)), add(intLit(3), intLit(4)))
  }

  type OptInterp[A] = Opt[Interp, A]

  println(p2[OptInterp].run(List()))

  type OptPrint[A] = Opt[Print, A]

  println(s"Pretty: ${p[Print].value}")
  println(s"OptPretty: ${p[OptPrint].run(List())._2.value}")
}
