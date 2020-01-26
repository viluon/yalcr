package yalcr

import yalcr.repl.Commands.Command
import yalcr.repl.Repl
import yalcr.repl.Repl.State
import yalcr.repl.eval.{ReducingStrategy, Strategy}

import scala.io.StdIn

object Main extends App {
  def lazyInput: LazyList[String] = {
    (StdIn readLine "λ ") #:: lazyInput
  }

  val repl = new Repl {
    override val evaluationStrategy: Strategy[State, (Command, String)] = ReducingStrategy

    override def print(state: State): Unit = state.errorOrOk match {
      case Left(err) => println(s"${Console.RED}error: $err${Console.RESET}")
      case Right(str) => println(str)
    }
  }

  repl loop lazyInput
}
