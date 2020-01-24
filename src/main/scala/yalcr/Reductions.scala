package yalcr

import LazyList.#::
import yalcr.lang.{EApplication, ELambda, EParam, Expression}

object Reductions {
  def eta(expr: Expression): Option[Expression] = expr match {
    case EApplication(λ, argument) => ???
    case ELambda(params, body) => ???
    case _ => None
  }

  def beta(expr: Expression, context: Map[EParam, Option[Expression]] = Map.empty): Option[Expression] = expr match {
    case ELambda(params, body) => beta(body, context removedAll params)
    case EApplication(ELambda(params, body), argument) =>
      val newBody = substitute(body, params.head.name, argument)
      Some(if (params.tail.nonEmpty) ELambda(params.tail, newBody) else newBody)

    case EApplication(lambda, argument) =>
      val reduced = LazyList(lambda, argument) map (e => (e, beta(e, context)))
      lazy val result = reduced map (t => t._2 getOrElse t._1) match {
        case lambda #:: argument #:: _ => EApplication(lambda, argument)
      }

      reduced find (_._2.nonEmpty) map (_ => result)
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
