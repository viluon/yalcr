package yalcr.evaluation

import yalcr.evaluation.Operations.Operation
import yalcr.lang._
import yalcr.parsing.Parser

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
  def beta(expr: Expression, scope: Map[Expression, Expression] = Map.empty, expandNumbers: Boolean = false): Option[Expression] = expr match {
    case _ if scope contains expr => scope get expr
    case ELambda(params, body) => for (body <- beta(body, scope removedAll params, expandNumbers)) yield ELambda(params, body)
    case EApplication(lambda, argument) =>
      val λDefaultAndReduced = lambda :: beta(lambda, scope, expandNumbers).toList
      val argDefaultAndReduced = argument :: beta(argument, scope, expandNumbers).toList

      (for (
        λ <- λDefaultAndReduced.reverse;
        arg <- argDefaultAndReduced.reverse
        if (λ == lambda) != (arg == argument)
      ) yield EApplication(λ, arg)).headOption orElse {
        lambda match {
          case ELambda(param :: ps, body) =>
            val expr = substitute(body, param, argument)
            Some(if (ps.nonEmpty) ELambda(ps, expr) else expr)
          case _ => None
        }
      }
    case n@ENumber(num) if expandNumbers && num >= 0 => Some(n.toLambda)
    case _ => None
  }

  def reduceAndExpand(expr: Expression, macros: Map[Expression, Expression] = Macros.all): Option[(Operation, Expression)] = {
    (beta(expr) map ((Operations.betaReduction, _))).orElse(
      beta(expr, macros, expandNumbers = true) map ((Operations.macroExpansion, _))
    )
  }

  def substitute(expr: Expression, param: EParam, argument: Expression): Expression = expr match {
    case `param` => argument
    case ELambda(params, _) if params contains param => expr
    case ELambda(params, body) => ELambda(params, substitute(body, param, argument))
    case EApplication(λ, arg) => EApplication(substitute(λ, param, argument), substitute(arg, param, argument))
    case x => x
  }
}
