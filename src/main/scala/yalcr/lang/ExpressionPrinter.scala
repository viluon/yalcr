package yalcr.lang

import scala.Console._

case class ExpressionPrinter(expr: Expression, depth: Int = 0, context: Set[EParam] = Set.empty) {
  private def nested(expr: Expression, deeper: Boolean = false, params: List[EParam] = Nil) = ExpressionPrinter(
    expr, if (deeper) depth + 1 else depth, params.foldLeft(context)((ctx, p) => ctx incl p)
  )

  lazy val pretty: String = expr match {
    case ENumber(num) => s"$num"
    case p@EParam(name) if context contains p => s"$name"
    case EParam(name) => s"$RED_B$name$RESET"

    case ELambda(params, body) => parens(s"λ ${params map (_.name) mkString ", "}. ${nested(body, deeper = true, params).pretty}")

    case EApplication(λ, argument: EApplication) => nested(λ).pretty + " " + parens(nested(argument, deeper = true).pretty)
    case EApplication(λ, argument) => nested(λ).pretty + " " + nested(argument).pretty
  }

  def parens(s: String): String = coloured("(") + s + coloured(")")

  def coloured(s: String): String = {
    val colour = List(GREEN
      , BLUE
      , YELLOW
      , MAGENTA
      , CYAN
      , WHITE)(depth % 6)

    s"$colour$s$RESET"
  }
}
