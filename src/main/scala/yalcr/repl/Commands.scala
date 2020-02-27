package yalcr.repl
import yalcr.lang.Expression

// TODO add example expressions
object Commands {
  def usage(cmd: Option[String] = None): String = cmd match {
    case Some(command) => command match {
      case "next" =>
        """Continue evaluation of a previously entered expression.
          |This command is equivalent to just pressing return (enter)
          |after an expression has been entered.
          |""".stripMargin
      case "contract" =>
        """Attempt to contract an expression.
          |Contraction is the inverse of macro expansion. The result of this
          |command is the transitive closure of contraction.
          |
          |:contract can either be applied to an argument (the expression to contract)
          |or to the last used expression (when no argument is given).
          |""".stripMargin
      case "def" =>
        """Define a custom macro.
          |The first argument is the expression to replace (typically a name for the macro),
          |the second argument is what to replace it with.
          |""".stripMargin
      case "help" =>
        """Print yalcr usage.
          |Try just ':help' (without an argument).
          |""".stripMargin
      case unknown => s"Unknown command: $unknown"
    }
    case None =>
      """Type an expression and press return (enter) to have it reduced.
        |
        |Available commands:
        |  :next                              Continue evaluation
        |  :contract [expression]             Attempt to reverse macro expansion
        |  :help [topic]                      Show help about a topic,
        |                                     or general help if no topic is given
        |  :def <expression> := <expression>  Define a custom macro
        |""".stripMargin
  }

  sealed trait Command

  case class Def(name: Expression, body: Expression) extends Command
  case class Help(command: Option[String]) extends Command
  case class Contract(expr: Option[Expression]) extends Command
  case class Solve(expr: Expression) extends Command
  case object Next extends Command
}
