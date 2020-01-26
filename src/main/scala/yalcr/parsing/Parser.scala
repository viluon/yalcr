package yalcr.parsing

import yalcr.lang._
import yalcr.repl.Commands
import yalcr.repl.Commands.Command

import scala.util.parsing.combinator.{PackratParsers, Parsers, RegexParsers}

object Parser extends Parsers with RegexParsers with PackratParsers {
  type P[+A] = PackratParser[A]

  // @formatter:off
  lazy val number: P[ENumber]      = "\\d+".r ^^ (_.toInt) ^^ ENumber
  lazy val param:  P[EParam]       = ("""[^`:\\λ.,()\s]+""".r | ("`" ~> "[^`]+".r <~ "`")) ^^ EParam
  lazy val params: P[List[EParam]] = ((param <~ ",".r).* ~ param ^^ (t => t._1 appended t._2)) <~ "."
  lazy val lambda: P[ELambda]      = "(" ~> ("λ" | "\\") ~> params ~ expression <~ ")" ^^ (x => ELambda(x._1, x._2))
  // @formatter:on

  lazy val term: P[Expression] = number | param | lambda | "(" ~> expression <~ ")"

  lazy val expression: P[Expression] = rep1(term) ^^ { es =>
    es.tail.foldLeft(es.head) {
      case (exp, acc) => EApplication(exp, acc)
    }
  }

  lazy val helpCmd: P[Commands.Help] = ":help" ~> ":".? ~> "\\w+".r.? ^^ Commands.Help
  lazy val nextCmd: P[Commands.Next.type] = ":next" ^^ (_ => Commands.Next)
  lazy val contractCmd: P[Commands.Contract] = ":contract" ~> expression.? ^^ (expr => Commands.Contract(expr))
  lazy val defCmd: P[Commands.Def] = ":def" ~> (expression <~ ":=") ~ expression ^^ (exprs => Commands.Def(exprs._1, exprs._2))
  lazy val command: P[Command] = helpCmd | nextCmd | contractCmd | defCmd | (expression ^^ Commands.Solve)

  def parseExpr(str: String): ParseResult[Expression] = parseAll(expression, str)

  def parse(str: String): ParseResult[Command] = parseAll(command, str)
}
