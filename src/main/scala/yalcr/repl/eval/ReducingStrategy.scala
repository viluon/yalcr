package yalcr.repl.eval

import yalcr.evaluation.Operations.Operation
import yalcr.evaluation.Reductions
import yalcr.extensions.Strings.newline
import yalcr.lang.{EApplication, ENumber, EParam, Expression, Macros}
import yalcr.parsing.Parser
import yalcr.repl.Commands.Command
import yalcr.repl.Repl.State
import yalcr.repl.{Commands, Repl, eval}

object ReducingStrategy extends eval.Strategy[State, (Command, String)] {
  override def eval(state: State, cmd: (Command, String)): State = state match {
    case Repl.State(lastExpr, history, _) =>
      val r: Either[String, (Expression, Option[(Operation, Expression)])] = (cmd match {
        case (Commands.solve, expr) => parse(expr)
        case (Commands.next, _) => lastExpr toRight "no computation to continue"
        case (Commands.help, _) => Left(Commands.usage)
      }) map (expr => (expr, Reductions.reduceAndExpand(expr, outMacros(history) ++ Macros.all)))

      val currentExpr = for (
        (_, reduced) <- r.toOption;
        (_, expr) <- reduced
      ) yield expr

      val errorOrOk = r map {
        case (original, reduced) => show(original, reduced, history.size)
      }

      Repl.State(currentExpr, currentExpr.toList appendedAll history, errorOrOk)
  }

  def outMacros(history: List[Expression]): Map[Expression, Expression] = {
    history.foldLeft[(Map[Expression, Expression], Int)]((Map(), history.size - 1)) {
      case ((map, n), expr) => (map.updated(EApplication(EParam("out"), ENumber(n).toLambda), expr), n - 1)
    }._1
  }

  def show(expr: Expression, reduced: Option[(Operation, Expression)], n: Int): String = reduced match {
    case Some((op, expr)) => op.toString + newline + s"out $n = " + expr.pretty
    case None => "no reductions possible"
  }

  def parse(expr: String): Either[String, Expression] = Parser parse expr match {
    case fail: Parser.NoSuccess => Left(fail.toString)
    case Parser.Success(expr, _) => Right(expr)
  }
}
