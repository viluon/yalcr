package yalcr.repl

import yalcr.extensions.Strings.newline
import yalcr.lang.{Expression, Macros}
import yalcr.parsing.Parser
import yalcr.repl.Commands.Command

import scala.util.{Failure, Success, Try}

trait Repl {
  import yalcr.repl.Repl._

  val evaluationStrategy: eval.Strategy[State, Command]

  def print(state: State): Unit

  @scala.annotation.tailrec
  final def loop(input: LazyList[String], init: State = initial): Result = {
    val State(lastExpr, history, _, macros) = init
    Try(input.headOption) match {
      case Failure(_) | Success(Some(null)) => Left(AbortReasons.Terminated)
      case Success(None) => Left(AbortReasons.ReachedEndOfInput)
      case Success(Some(ws)) if "^\\s*$".r matches ws => loop(input.tail, lastExpr match {
        case None => init
        case _ => val s = evaluationStrategy.eval(init, Commands.Next)
          print(s)
          s
      })
      case Success(Some(str)) =>
        val state = Parser parse str match {
          case fail: Parser.NoSuccess =>
            State(lastExpr, history, Left(fail.toString + newline + Commands.usage()), macros)
          case Parser.Success(result, _) =>
            evaluationStrategy.eval(init, result)
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

  case class State(lastExpr: Option[Expression], history: List[Expression], errorOrOk: Either[String, String], macros: Map[Expression, Expression])

  val initial = State(None, Nil, Right(""), Macros.all)
}
