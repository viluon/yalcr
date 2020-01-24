package yalcr

import yalcr.lang.{EApplication, ELambda, EParam, Expression}

object Reductions {
  def beta(expr: Expression, context: Map[EParam, Option[Expression]] = Map.empty): Option[Expression] = expr match {
    case ELambda(params, body) => beta(body, context removedAll params)
    case EApplication(ELambda(params, body), argument) => val newBody = substitute(body, params.head.name, argument)
      Some(if (params.tail.nonEmpty) ELambda(params.tail, newBody) else newBody)
    case EApplication(unknown, argument) => beta(argument, context) match {
      case Some(reduced) => Some(EApplication(unknown, reduced))
      case None => None
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
