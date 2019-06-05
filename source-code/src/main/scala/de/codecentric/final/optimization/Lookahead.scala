package de.codecentric.`final`.optimization

import cats.data.Tuple2K
import de.codecentric.`final`.ExprSym

object Lookahead {
  implicit def tupleExprSym[F[_], G[_]](
      implicit F: ExprSym[F],
      G: ExprSym[G]): ExprSym[Tuple2K[F, G, ?]] =
    new ExprSym[Tuple2K[F, G, ?]] {
      override def intLit(value: Int): Tuple2K[F, G, Int] =
        Tuple2K(F.intLit(value), G.intLit(value))

      override def add(e1: Tuple2K[F, G, Int],
                       e2: Tuple2K[F, G, Int]): Tuple2K[F, G, Int] =
        Tuple2K(F.add(e1.first, e2.first), G.add(e1.second, e2.second))

      override def strLit(value: String): Tuple2K[F, G, String] =
        Tuple2K(F.strLit(value), G.strLit(value))

      override def concat(e1: Tuple2K[F, G, String],
                          e2: Tuple2K[F, G, String]): Tuple2K[F, G, String] =
        Tuple2K(F.concat(e1.first, e2.first), G.concat(e1.second, e2.second))

      override def strToInt(e: Tuple2K[F, G, String]): Tuple2K[F, G, Int] =
        Tuple2K(F.strToInt(e.first), G.strToInt(e.second))
    }

  def lookahead[F[_]](
      implicit base: ExprSym[F]): ExprSym[Tuple2K[Thunk[F, ?], Peek, ?]] =
    new ExprSym[Tuple2K[Thunk[F, ?], Peek, ?]] {
      override def intLit(value: Int): Tuple2K[Thunk[F, ?], Peek, Int] =
        Tuple2K(Thunk(() => base.intLit(value)), Peek.IntLit(value))

      override def add(e1: Tuple2K[Thunk[F, ?], Peek, Int],
                       e2: Tuple2K[Thunk[F, ?], Peek, Int])
        : Tuple2K[Thunk[F, ?], Peek, Int] = {

        (e1.second, e2.second) match {
          case (Peek.IntLit(lhs), Peek.IntLit(rhs)) =>
            Tuple2K(Thunk(() => base.intLit(lhs + rhs)), Peek.IntLit(lhs + rhs))
          case _ =>
            Tuple2K(Thunk(() => base.add(e1.first(), e2.first())), Peek.Add)
        }
      }

      override def strLit(value: String): Tuple2K[Thunk[F, ?], Peek, String] =
        Tuple2K(Thunk(() => base.strLit(value)), Peek.StrLit(value))

      override def concat(e1: Tuple2K[Thunk[F, ?], Peek, String],
                          e2: Tuple2K[Thunk[F, ?], Peek, String])
        : Tuple2K[Thunk[F, ?], Peek, String] =
        Tuple2K(Thunk(() => base.concat(e1.first(), e2.first())), Peek.Concat)

      override def strToInt(e: Tuple2K[Thunk[F, ?], Peek, String])
        : Tuple2K[Thunk[F, ?], Peek, Int] =
        Tuple2K(Thunk(() => base.strToInt(e.first())), Peek.StrToInt)
    }
}
