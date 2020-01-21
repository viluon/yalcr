package yalcr.repl.eval

import yalcr.Reductions
import yalcr.extensions.Monad
import yalcr.parsing.Parser
import yalcr.repl.Commands
import yalcr.repl.Commands.Command
import yalcr.repl.state.{ReplState, ReplStateMonad}

object ReducingStrategy extends Strategy[ReplState] {
  override val stateMonad: Monad[ReplState] = ReplStateMonad

  override def eval[A, B](state: ReplState[A], cmd: (Command, String)): ReplState[B] = {
    import scala.language.reflectiveCalls

    cmd._1 match {
      case Commands.solve => ???
      case Commands.help => (??? : {def foo(x: String): Nothing}).foo(Commands.usage)
    }
  }

  def solve(expr: String): String = {
    Parser.parseAll(Parser.expression, expr) match {
      case fail: Parser.NoSuccess => fail.toString
      case Parser.Success(expr, _) =>
        val reductionResult = Reductions beta expr match {
          case Some(reduced) => reduced.pretty()
          case None => "None"
        }
        List(expr.pretty(), reductionResult) mkString System.lineSeparator()
    }
  }
}
