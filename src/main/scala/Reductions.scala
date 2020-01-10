
object Reductions {
  // None represents the impossibility of beta reduction
  // other issues are reported with a readable string
  type Failure = Option[String]

  def beta(expr: Expression, context: Map[String, Option[Expression]] = Map.empty): Either[Failure, Expression] = expr match {
    case ELambda(_, body) => beta(body, context)
    case EApplication(λ1, param) => beta(λ1) match {
      case err : Left[_, _] => // attempt to recover with application
        ???
      case Right(value) => ???
    }
    case _ => Left(None)
  }

  def substitute(expr: Expression, name: String, value: Expression): Expression = expr match {
    case EParam(`name`) => value
    case ELambda(params, _) if params contains name => expr
    case ELambda(params, body) => ELambda(params, substitute(body, name, value))
    case EApplication(λ, param) => EApplication(substitute(λ, name, value), substitute(param, name, value))
    case x => x
  }
}


/*
λ1 match {
      case ELambda(params, body) => Right(ELambda(params, substitute(body, params.head, param)))
      case EApplication(λ2, param) => beta(λ2) match {
        case e: Left[_, _] => e
        case Right(λ3) => Right(EApplication(λ3, param))
      }
      case _ => Left(s"cannot reduce $λ1")
    }
 */