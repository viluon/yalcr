package yalcr.repl

import yalcr.repl.Commands.Command

import scala.util.parsing.combinator.{Parsers, RegexParsers}

object CommandParser extends Parsers with RegexParsers {
  // @formatter:off
  def solve: Parser[Command] = "solve" ^^ (_ => Commands.solve)
  def help:  Parser[Command] = "help"  ^^ (_ => Commands.help)
  // @formatter:on

  def command: Parser[Command] = solve | help

  def parse(input: String): ParseResult[Command] = parse(command, input)
}
