package yalcr.repl

import yalcr.lang.Expression
import yalcr.parsing.CommandParser
import yalcr.repl.Commands.Command
import yalcr.extensions.Strings.newline

import scala.util.{Failure, Success, Try}

trait Repl {
  import yalcr.repl.Repl._

  val evaluationStrategy: eval.Strategy[State, (Command, String)]

  def print(state: State)

  @scala.annotation.tailrec
  final def loop(input: LazyList[String], init: State = initial): Result = {
    val State(lastExpr, history, _) = init
    Try(input.headOption) match {
      case Failure(_) | Success(Some(null)) => Left(AbortReasons.Terminated)
      case Success(None) => Left(AbortReasons.ReachedEndOfInput)
      case Success(Some(str)) =>
        val state = CommandParser parse str match {
          case fail: CommandParser.NoSuccess =>
            State(lastExpr, history, Left(fail + newline + Commands.usage))
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
  type Result = Either[AbortReason, State]

  case class State(lastExpr: Option[Expression], history: List[Expression], errorOrOk: Either[String, String])

  val initial = State(None, Nil, Right(""))
}
