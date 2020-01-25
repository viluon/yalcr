package yalcr

import yalcr.lang.{EApplication, ELambda, EParam, Expression}

object Reductions {
  def beta(expr: Expression, context: Map[EParam, Option[Expression]] = Map.empty): Option[Expression] = expr match {
    case ELambda(params, body) => for (body <- beta(body, context removedAll params)) yield ELambda(params, body)
    case EApplication(lambda, argument) =>
      // (someLambda, someArgument)
      //  reduced     reduced      // prohibited (single step at a time)
      //  reduced     default
      //  default     reduced
      //  default     default      // prohibited (return None if not reducible)

      val λDefaultAndReduced = lambda :: beta(lambda, context).toList
      val argDefaultAndReduced = argument :: beta(argument, context).toList

      (for (
        λ <- λDefaultAndReduced.reverse;
        arg <- argDefaultAndReduced.reverse
        if (λ == lambda) != (arg == argument)
      ) yield EApplication(λ, arg)).headOption orElse {
        lambda match {
          case ELambda(params, body) =>
            val newBody = substitute(body, params.head.name, argument)
            Some(if (params.tail.nonEmpty) ELambda(params.tail, newBody) else newBody)
          case _ => None
        }
      }
    case _ => None
  }

  def substitute(expr: Expression, name: String, value: Expression): Expression = expr match {
    case EParam(`name`) => value
    case ELambda(params, _) if params contains name => expr
    case ELambda(params, body) => ELambda(params, substitute(body, name, value))
    case EApplication(λ, argument) => EApplication(substitute(λ, name, value), substitute(argument, name, value))
    case x => x
  }
}
