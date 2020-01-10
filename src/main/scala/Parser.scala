import scala.util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {
  // @formatter:off
  def number: Parser[Int]          = "\\d+".r ^^ (_.toInt)
  def param:  Parser[String]       = "\\w+".r ^^ (_.toString)
  def params: Parser[List[String]] = ((param <~ "\\s+".r).* ~ param ^^ (t => t._1 appended t._2)) <~ "."
  def lambda: Parser[ELambda]      = """\([\\λ]""".r ~> params ~ expression <~ ")" ^^ (x => ELambda(x._1, x._2))
  // @formatter:on

  def expression: Parser[Expression] = atom | ("(" ~> atom <~ ")")

  def atom: Parser[Expression] = (number | param | lambda) ~ expression.? ^^ { compound =>
    val first = compound._1 match {
      case x: Int => ENumber(x)
      case x: String => EParam(x)
      case x: Expression => x
      case x => throw new IllegalStateException(s"what? $x")
    }

    compound._2 match {
      case Some(arg) => EApplication(first, arg)
      case None => first
    }
  }
}
