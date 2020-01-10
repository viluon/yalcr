sealed trait Expression

case class ENumber(num: Int) extends Expression {
  override def toString: String = s"$num"
}
case class EParam(name: String) extends Expression {
  override def toString: String = s"$name"
}
case class ELambda(params: List[String], body: Expression) extends Expression {
  override def toString: String = s"(λ ${params.mkString(" ")}. $body)"
}
case class EApplication(λ: Expression, param: Expression) extends Expression {
  override def toString: String = s"$λ $param"
}
