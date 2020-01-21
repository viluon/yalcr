package yalcr.repl

import yalcr.extensions.Monad

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

trait Repl {
  import yalcr.repl.Repl._

  type Result[A] = Either[AbortReason, State[A]]
  type State[A]

  val stateMonad: Monad[State]
  val evaluationStrategy: eval.Strategy[State]

  def print[A](state: State[A])
  def printError(err: String)

  @scala.annotation.tailrec
  final def loop[A, B](input: LazyList[String], init: State[A]): Result[B] = {
    Try(input.headOption) match {
      case Failure(_) => Left(AbortReasons.Terminated)
      case Success(None) => Left(AbortReasons.ReachedEndOfInput)
      case Success(Some(str)) =>
        CommandParser parse str.toLowerCase() match {
          case fail: CommandParser.NoSuccess =>
            printError(s"$fail${System.lineSeparator()}${Commands.usage}")
            loop(input.tail, init)
          case CommandParser.Success(result, next) =>
            val state = evaluationStrategy.eval(init, (result, next.source.toString))
            print(state)
            loop(input.tail, state)
        }
    }
  }
}

object Repl {
  object AbortReasons extends Enumeration {
    val ReachedEndOfInput: AbortReason = Value
    val Terminated: AbortReason = Value
  }

  type AbortReason = AbortReasons.Value
}
