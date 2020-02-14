import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import yalcr.extensions.Strings.newline
import yalcr.lang.ENumber
import yalcr.repl.eval.{ReducingStrategy, Strategy}
import yalcr.repl.{Commands, Repl}

class FactorialTest extends AnyFlatSpec with Matchers {
  def newlines: LazyList[String] = newline #:: newlines

  val repl: Repl = new Repl {
    override val evaluationStrategy: Strategy[Repl.State, Commands.Command] = ReducingStrategy
    override def print(state: Repl.State): Unit = ()
  }

  "repl" should "evaluate the factorial of 3" in {
    val (state, _) = repl loop ("Y (λf, n. zero n 1 (* n (f (pred n)))) 3" #:: newlines take 833)
    state.lastExpr shouldBe Some(ENumber(6).toLambda)
  }
}
