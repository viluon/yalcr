package yalcr.lang

sealed trait Expression {
  val pretty: String = ExpressionPrinter(this).pretty
}

case class ENumber(num: Int) extends Expression
case class EParam(name: String) extends Expression
case class ELambda(params: List[EParam], body: Expression) extends Expression
case class EApplication(λ: Expression, argument: Expression) extends Expression
