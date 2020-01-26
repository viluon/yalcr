package yalcr.lang

import yalcr.parsing.Parser

object Macros {
  private val parseMacroPair: ((String, String)) => (Expression, Expression) = {
    case (key, value) => (EParam(key), (Parser parseExpr value).get)
  }

  val arithmetic: Map[Expression, Expression] = Map(
    ("+", "add"),
    ("*", "mult"),
    ("add", "(λ x, y, s, z. x s (y s z))"),
    ("mult", "(λ x, y, s, z. x (y s) z)"),
    ("pred", "(λ x, s, z. x (λ f, g. g (f s)) (λ g. z) (λ m. m))"),
  ) map parseMacroPair

  val logic: Map[Expression, Expression] = Map(
    ("T", "true"),
    ("F", "false"),
    ("&&", "and"),
    ("||", "or"),
    ("true", "(λ t, f. t)"),
    ("false", "(λ t, f. f)"),
    ("not", "(λ x, t, f. x f t)"),
    ("and", "(λ x, y. x y false)"),
    ("or", "(λ x, y. x true y)"),
  ) map parseMacroPair

  val functional: Map[Expression, Expression] = Map(
    ("zero", "(λ x. x false not false)"),
    ("Y", "(λ f. (λ x. f (x x)) (λ x. f (x x)))"),
  ) map parseMacroPair

  lazy val all: Map[Expression, Expression] = arithmetic ++ logic ++ functional
}
