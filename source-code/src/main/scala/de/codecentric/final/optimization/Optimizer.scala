package de.codecentric.`final`.optimization

import de.codecentric.`final`.ExprSym
import de.codecentric.`final`.optimization.Ctx._

//snippet:final-tagless-opt-ctx
sealed abstract class Ctx // explicit context needed
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
  // Using kind-projector
  implicit def inlineAdditionExprSym[F[_]](
      implicit base: ExprSym[F]): ExprSym[Opt[F, ?]] = ???
  //snippet:end
}

object Opt {
  implicit def inlineAdditionExprSym[F[_]](
      implicit base: ExprSym[F]): ExprSym[Opt[F, ?]] = {
    new ExprSym[Opt[F, ?]] {

      //snippet:final-tagless-opt-impl
// def inlineAdditionExprSym[F[_])(...) = {
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

      // more overrides...
// }
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