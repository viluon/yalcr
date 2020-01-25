package yalcr.lang

sealed trait Expression {
  val pretty: String = ExpressionPrinter(this).pretty
}

case class EParam(name: String) extends Expression
case class ELambda(params: List[EParam], body: Expression) extends Expression
case class EApplication(λ: Expression, argument: Expression) extends Expression
case class ENumber(num: Int) extends Expression {
  lazy val toLambda: ELambda = {
    def chain(n: Int = num): Expression = n match {
      case 0 => EParam("z")
      case _ => EApplication(EParam("s"), chain(n - 1))
    }

    ELambda(List(EParam("s"), EParam("z")), chain())
  }
}
