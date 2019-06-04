package de.codecentric.`final`.optimization

import de.codecentric.`final`.ExprSym

/*
 * Wrap an regular ExprSym interpreter and "delay" the actual evaluation
 * Useful in combination with Peek and Tuple2K to have a lookahead
 */
case class Thunk[F[_], A](value: () => F[A]) {
  def apply(): F[A] = value()
}

object Thunk {
  implicit def thunkExprSym[F[_]](
      implicit F: ExprSym[F]): ExprSym[Thunk[F, ?]] =
    new ExprSym[Thunk[F, ?]] {
      override def intLit(value: Int): Thunk[F, Int] =
        Thunk(() => F.intLit(value))

      override def add(e1: Thunk[F, Int], e2: Thunk[F, Int]): Thunk[F, Int] =
        Thunk(() => F.add(e1.value(), e2.value()))

      override def strLit(value: String): Thunk[F, String] =
        Thunk(() => F.strLit(value))

      override def concat(e1: Thunk[F, String],
                          e2: Thunk[F, String]): Thunk[F, String] =
        Thunk(() => F.concat(e1.value(), e2.value()))

      override def strToInt(e: Thunk[F, String]): Thunk[F, Int] =
        Thunk(() => F.strToInt(e.value()))
    }
}
