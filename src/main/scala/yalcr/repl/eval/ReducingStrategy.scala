package yalcr.repl.eval

import yalcr.Reductions
import yalcr.lang.Expression
import yalcr.parsing.Parser
import yalcr.repl.Commands
import yalcr.repl.Commands.Command
import yalcr.repl.Repl.State

object ReducingStrategy extends Strategy[State, (Command, String)] {
  override def eval(state: State, cmd: (Command, String)): State = state match {
    case (history, _) => (history, cmd match {
      case (Commands.solve, expr) => solve(expr) match {
        case Left(err) => Left(err)
        case Right((expr, maybeReduced)) =>
          val newline = System.lineSeparator()
          Right(expr.pretty() + newline + (maybeReduced match {
            case Some(reduced) => s"β -> ${reduced.pretty()}"
            case None => "no reductions possible"
          }))
      }
      case (Commands.help, _) => Left(Commands.usage)
    })
  }

  def solve(expr: String): Either[String, (Expression, Option[Expression])] = {
    Parser.parseAll(Parser.expression, expr) match {
      case fail: Parser.NoSuccess => Left(fail.toString)
      case Parser.Success(expr, _) => Right((expr, Reductions beta expr))
    }
  }
}
