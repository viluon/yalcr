package yalcr.repl.state

import yalcr.extensions.Monad

case class ReplState[A](x: A)

object ReplStateMonad extends Monad[ReplState] {
  override def flatMap[A, B](x: ReplState[A])(λ: A => ReplState[B]): ReplState[B] = λ(x.x)
  override def filter[A](x: ReplState[A])(p: A => Boolean): ReplState[A] = throw new NotImplementedError
  override def ret[A](x: A): ReplState[A] = ReplState(x)
}
