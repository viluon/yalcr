
import scala.language.implicitConversions
import scala.util.parsing.combinator.{PackratParsers, Parsers, RegexParsers}

object Parser extends Parsers with RegexParsers with PackratParsers {
  type P[+A] = PackratParser[A]

  // @formatter:off
  lazy val number: P[ENumber]      = "\\d+".r ^^ (_.toInt) ^^ ENumber
  lazy val param:  P[EParam]       = """[^\\λ.,()\s]+""".r ^^ EParam
  lazy val params: P[List[EParam]] = ((param <~ ",".r).* ~ param ^^ (t => t._1 appended t._2)) <~ "."
  lazy val lambda: P[ELambda]      = "(" ~> ("λ" | "\\") ~> params ~ expression <~ ")" ^^ (x => ELambda(x._1, x._2))
  // @formatter:on

  lazy val term: P[Expression] = number | param | lambda | "(" ~> expression <~ ")"

  lazy val expression: P[Expression] = rep1(term) ^^ { es =>
    es.tail.foldLeft(es.head) {
      case (exp, acc) => EApplication(exp, acc)
    }
  }
}
