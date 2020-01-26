import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import yalcr.lang.{EApplication, ENumber, EParam}
import yalcr.parsing.Parser
import yalcr.repl.Commands

class ParserTest extends AnyFlatSpec with Matchers {
  "parser" should "parse free expressions" in {
    (Parser parse "+ 1 2").get should be (Commands.Solve(EApplication(EApplication(EParam("+"), ENumber(1)), ENumber(2))))
  }

  it should "recognise special commands" in {
    (Parser parse ":next").get should be (Commands.Next)
  }
}
