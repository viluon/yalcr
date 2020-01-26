package yalcr.lang

import scala.Console._

case class ExpressionPrinter(expr: Expression, depth: Int = 0, scope: Set[Expression] = Macros.all.keySet incl EParam("out")) {
  private def nested(expr: Expression, deeper: Boolean = false, params: List[EParam] = Nil) = ExpressionPrinter(
    expr, if (deeper) depth + 1 else depth, params.foldLeft(scope)((ctx, p) => ctx incl p)
  )

  lazy val pretty: String = expr match {
    case ENumber(num) => s"$num"
    case p@EParam(name) if scope contains p => BOLD + name + RESET
    case EParam(name) => s"$RED_B$name$RESET"

    case ELambda(params, body) => parens(s"λ ${params map (BOLD + _.name + RESET) mkString ", "}. ${nested(body, deeper = true, params).pretty}")

    case EApplication(λ, argument: EApplication) => nested(λ).pretty + " " + parens(nested(argument, deeper = true).pretty)
    case EApplication(λ, argument) => nested(λ).pretty + " " + nested(argument).pretty
  }

  def parens(s: String): String = coloured("(") + s + coloured(")")

  def coloured(s: String): String = {
    // @formatter:off
    val colour = List(GREEN
                    , BLUE
                    , YELLOW
                    , MAGENTA
                    , CYAN
                    , WHITE
                     )(depth % 6)
    // @formatter:on

    s"$colour$s$RESET"
  }
}
