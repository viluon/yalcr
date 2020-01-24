package yalcr.repl.eval

import yalcr.Reductions
import yalcr.extensions.Strings.newline
import yalcr.lang.Expression
import yalcr.parsing.Parser
import yalcr.repl.Commands.Command
import yalcr.repl.Repl.State
import yalcr.repl.{Commands, eval}

object ReducingStrategy extends eval.Strategy[State, (Command, String)] {
  override def eval(state: State, cmd: (Command, String)): State = state match {
    case (lastExpr, history, _) =>
      val r: Either[String, Expression] = cmd match {
        case (Commands.solve, expr) => parse(expr)
        case (Commands.next, _) => lastExpr toRight "no computation to continue"
        case (Commands.help, _) => Left(Commands.usage)
      }
      // FIXME ugly double reduction
      (r.toOption.flatMap(e => Reductions beta e), history, r.map(e => pretty(e, Reductions beta e)))
  }

  def pretty(expr: Expression, reduced: Option[Expression]): String = expr.pretty + newline + (reduced match {
    case Some(reduced) => s"β -> ${reduced.pretty}"
    case None => "no reductions possible"
  })

  def parse(expr: String): Either[String, Expression] = Parser.parseAll(Parser.expression, expr) match {
    case fail: Parser.NoSuccess => Left(fail.toString)
    case Parser.Success(expr, _) => Right(expr)
  }
}
