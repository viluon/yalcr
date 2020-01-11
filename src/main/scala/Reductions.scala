
object Reductions {
  @scala.annotation.tailrec
  def beta(expr: Expression, context: Map[EParam, Option[Expression]] = Map.empty): Option[Expression] = expr match {
    case ELambda(params, body) => beta(body, context removedAll params)
    case EApplication(ELambda(params, body), value) => val newBody = substitute(body, params.head.name, value)
      Some(if (params.tail.nonEmpty) ELambda(params.tail, newBody) else newBody)
    case _ => None
  }

  def substitute(expr: Expression, name: String, value: Expression): Expression = expr match {
    case EParam(`name`) => value
    case ELambda(params, _) if params contains name => expr
    case ELambda(params, body) => ELambda(params, substitute(body, name, value))
    case EApplication(λ, param) => EApplication(substitute(λ, name, value), substitute(param, name, value))
    case x => x
  }
}
