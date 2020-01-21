package yalcr.repl.eval

import yalcr.extensions.Monad
import yalcr.repl.Commands.Command

trait Strategy[State[_]] {
  val stateMonad: Monad[State]

  def eval[A, B](state: State[A], cmd: (Command, String)): State[B]
}
