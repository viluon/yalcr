package yalcr.repl

import yalcr.lang.Expression
import yalcr.parsing.CommandParser
import yalcr.repl.Commands.Command

import scala.util.{Failure, Success, Try}

// TODO In[n] and Out[n] for the REPL, reference previous results, indicate what operation led to the result

trait Repl {
  import yalcr.repl.Repl._

  val evaluationStrategy: eval.Strategy[State, (Command, String)]

  def print(state: State)

  @scala.annotation.tailrec
  final def loop(input: LazyList[String], init: State): Result = {
    val (lastExpr, history, _) = init
    Try(input.headOption) match {
      case Failure(_) | Success(Some(null)) => Left(AbortReasons.Terminated)
      case Success(None) => Left(AbortReasons.ReachedEndOfInput)
      case Success(Some(str)) =>
        val state = CommandParser parse str.toLowerCase() match {
          case fail: CommandParser.NoSuccess =>
            (lastExpr, str :: history, Left(s"$fail${System.lineSeparator()}${Commands.usage}"))
          case CommandParser.Success(result, next) =>
            evaluationStrategy.eval(init, (result, next.source.toString.substring(next.offset)))
        }

        print(state)
        loop(input.tail, state)
    }
  }
}

object Repl {
  object AbortReasons extends Enumeration {
    val ReachedEndOfInput: AbortReason = Value
    val Terminated: AbortReason = Value
  }

  type AbortReason = AbortReasons.Value

  type History = List[String]
  type ErrorOrOk = Either[String, String]
  type LastExpression = Option[Expression]
  type State = (LastExpression, History, ErrorOrOk)
  type Result = Either[AbortReason, State]
}
