package yalcr.repl.eval

import yalcr.evaluation.Operations.Operation
import yalcr.evaluation.Reductions
import yalcr.extensions.Strings.newline
import yalcr.lang._
import yalcr.parsing.Parser
import yalcr.repl.Commands.Command
import yalcr.repl.Repl.State
import yalcr.repl.{Commands, Repl, eval}

object ReducingStrategy extends eval.Strategy[State, Command] {
  override def eval(state: State, cmd: Command): State = state match {
    case Repl.State(lastExpr, history, _, macros) =>
      def reduceAndShow(errorOrExpr: Either[String, Expression]): (List[Expression], Either[String, String]) = {
        val reduced = errorOrExpr map (expr => (expr, Reductions.reduceAndExpand(expr, macros)))
        (for ((_, r) <- reduced.toOption.toList; (_, e) <- r) yield e, reduced map show(history.size, macros))
      }

      val ((resultExprs, errorOrOk), newMacros) = cmd match {
        case Commands.Solve(expr) => (reduceAndShow(Right(expr)), None)
        case Commands.Next => (reduceAndShow(lastExpr toRight "no computation to continue"), None)
        case Commands.Help(cmd) => ((Nil, Right(Commands.usage(cmd))), None)
        case Commands.Contract(expr) => (contract(expr, lastExpr, macros, history), None)
        case Commands.Def(name, body) => val updated = macros.updated(name, body)
          ((Nil, Right(s"defined new macro ${name pretty updated.keySet}")), Some(updated))
      }

      val currentExpr = resultExprs match {
        case expr :: Nil => Some(expr)
        case _ => None
      }

      val combinedMacros = newMacros getOrElse macros
      val scope = currentExpr match {
        case Some(expr) => combinedMacros.updated(EApplication(EParam("out"), ENumber(history.size).toLambda), expr)
        case None => combinedMacros
      }
      Repl.State(currentExpr, resultExprs appendedAll history, errorOrOk, scope)
  }

  type ?[+A] = Option[A]
  type ->[A, +B] = Map[A, B]
  type EvalResult = (List[Expression], Either[String, String])

  private def contract(expr: ?[Expression], lastExpr: ?[Expression], macros: Expression -> Expression, history: List[Expression]): EvalResult = {
    val errorOrResults = expr orElse lastExpr toRight "no computation to continue" map {
      expr => Reductions.contract(expr, Reductions invertMap macros)
    }

    val first100 = errorOrResults map (_.take(100).zipWithIndex map {
      case (expr, i) => s"out ${i + history.size} = " + ExpressionPrinter(expr, macros.keySet).pretty
    } mkString newline)

    (errorOrResults.map(_.take(100)).toSeq.flatten.toList, errorOrResults flatMap {
      case LazyList() => Left("could not contract")
      case exprs if (exprs lengthCompare 100) > 0 => first100 flatMap {
        str => Left(s"contraction did not terminate in time, showing first 100 results$newline$str")
      }
      case _ => first100
    })
  }

  def show(n: Int, macros: Map[Expression, Expression])(p: (Expression, Option[(Operation, Expression)])): String = p match {
    case (_, Some((op, expr))) => op.toString + newline + s"out $n = " + ExpressionPrinter(expr, macros.keySet).pretty
    case (_, None) => "no reductions possible"
  }

  def parse(expr: String): Either[String, Expression] = Parser parseExpr expr match {
    case fail: Parser.NoSuccess => Left(fail.toString)
    case Parser.Success(expr, _) => Right(expr)
  }
}
