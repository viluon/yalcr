package yalcr.evaluation

import yalcr.evaluation.Operations.Operation
import yalcr.lang._

object Reductions {
  /** Reduce an expression by beta reduction.
   * When reducing function application, we prefer to reduce inner expressions first (the lambda and the argument),
   * starting from the left (i.e. preferring to first reduce the lambda). If neither of these inner expressions
   * can be reduced, we substitute the argument in the lambda. If substitution isn't possible, we attempt to expand
   * macros.
   *
   * Therefore, for a tuple of a lambda and an argument, we choose the highest possible allowed entry in the following table:
   * (someLambda, someArgument)
   * reduced     reduced       // prohibited (single step at a time)
   * reduced     default
   * default     reduced
   * default     default       // prohibited (return None if not reducible)
   *
   * @param expr  The expression to reduce.
   * @param scope The current scope for parameter lookups.
   * @return The result of beta reduction, or [[None]] if no reductions are possible.
   */
  def beta(expr: Expression, scope: Map[Expression, Expression] = Map.empty, expandNumbers: Boolean = false): Option[(Operation, Expression)] = expr match {
    case _ if scope contains expr => scope get expr map ((Operations.macroExpansion, _))
    case EApplication(ELambda(param :: ps, body), argument) =>
      val expr = substitute(body, param, argument)
      Some(Operations.betaReduction, if (ps.nonEmpty) ELambda(ps, expr) else expr)
    case ELambda(params, body) => for ((op, body) <- beta(body, scope removedAll params, expandNumbers)) yield (op, ELambda(params, body))
    case EApplication(lambda, argument) =>
      val λDefaultAndReduced = (Operations.identity, lambda) :: beta(lambda, scope, expandNumbers).toList
      val argDefaultAndReduced = (Operations.identity, argument) :: beta(argument, scope, expandNumbers).toList

      (for (
        (op1, λ) <- λDefaultAndReduced.reverse;
        (op2, arg) <- argDefaultAndReduced.reverse
        if (λ == lambda) != (arg == argument)
      ) yield (List(op1, op2).find(_ != Operations.identity).get, EApplication(λ, arg))).headOption
    case n@ENumber(num) if expandNumbers && num >= 0 => Some(Operations.macroExpansion, n.toLambda)
    case _ => None
  }

  def reduceAndExpand(expr: Expression, macros: Map[Expression, Expression] = Macros.all): Option[(Operation, Expression)] = {
    beta(expr, macros, expandNumbers = true)
  }

  def invertMap[K, V](map: Map[K, V]): Map[V, List[K]] = {
    map.foldLeft(Map[V, List[K]]()) {
      case (map, (k, v)) => (map updatedWith v) {
        maybeKs => Some(k :: maybeKs.toList.flatten)
      }
    }
  }

  def packNumber(s: EParam, z: EParam, chain: Expression): Option[ENumber] = chain match {
    case `z` => Some(ENumber(0))
    case EApplication(`s`, tail) => packNumber(s, z, tail) map (n => ENumber(n.num + 1))
    case _ => None
  }

  def contract(expr: Expression, inverseMacros: Map[Expression, List[Expression]]): LazyList[Expression] = {
    contractOnce(expr, inverseMacros) flatMap (expr => expr #:: contract(expr, inverseMacros))
  }

  def contractOnce(expr: Expression, inverseMacros: Map[Expression, List[Expression]]): LazyList[Expression] = {
    (LazyList from (inverseMacros get expr).toList.flatten) appendedAll (expr match {
      case ELambda(s :: z :: Nil, chain) if packNumber(s, z, chain).nonEmpty => packNumber(s, z, chain).toList
      case ELambda(params, body) => for (body <- contract(body, inverseMacros)) yield ELambda(params, body)
      case EApplication(λ, argument) =>
        for (
          (lambda, i) <- (λ #:: contract(λ, inverseMacros)).zipWithIndex;
          (arg, j) <- (argument #:: contract(argument, inverseMacros)).zipWithIndex
          if i + j > 0
        ) yield EApplication(lambda, arg)
      case _ => Nil
    })
  }

  def substitute(expr: Expression, param: EParam, argument: Expression): Expression = expr match {
    case `param` => argument
    case ELambda(params, _) if params contains param => expr
    case ELambda(params, body) => ELambda(params, substitute(body, param, argument))
    case EApplication(λ, arg) => EApplication(substitute(λ, param, argument), substitute(arg, param, argument))
    case x => x
  }
}
