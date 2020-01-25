package yalcr

import yalcr.lang.{EApplication, ELambda, ENumber, EParam, Expression}
import yalcr.parsing.Parser

object Reductions {
  val macros: Map[String, Expression] = Map(
    ("+", "(λ x, y, s, z. x s (y s z))"),
  ) map {
    case (key, value) => (key, Parser.parseAll(Parser.expression, value).get)
  }

  // TODO start with scope = macros; remove substitute() (beta() should be enough)
  // FIXME too eager to expand macros right now, see `solve + (+ 1 1) 1`
  def beta(expr: Expression, scope: Map[EParam, Option[Expression]] = Map.empty): Option[Expression] = expr match {
    case ELambda(params, body) => for (body <- beta(body, scope removedAll params)) yield ELambda(params, body)
    case EApplication(lambda, argument) =>
      // (someLambda, someArgument)
      //  reduced     reduced      // prohibited (single step at a time)
      //  reduced     default
      //  default     reduced
      //  default     default      // prohibited (return None if not reducible)

      val λDefaultAndReduced = lambda :: beta(lambda, scope).toList
      val argDefaultAndReduced = argument :: beta(argument, scope).toList

      (for (
        λ <- λDefaultAndReduced.reverse;
        arg <- argDefaultAndReduced.reverse
        if (λ == lambda) != (arg == argument)
      ) yield EApplication(λ, arg)).headOption orElse {
        lambda match {
          case ELambda(param :: ps, body) =>
            val newBody = substitute(body, param, argument)
            Some(if (ps.nonEmpty) ELambda(ps, newBody) else newBody)
          case _ => None
        }
      }
    case n@ENumber(num) if num >= 0 => Some(n.toLambda)
    case EParam(name) => macros get name
    case _ => None
  }

  def substitute(expr: Expression, name: EParam, value: Expression): Expression = expr match {
    case `name` => value
    case ELambda(params, _) if params contains name => expr
    case ELambda(params, body) => ELambda(params, substitute(body, name, value))
    case EApplication(λ, argument) => EApplication(substitute(λ, name, value), substitute(argument, name, value))
    case x => x
  }
}
