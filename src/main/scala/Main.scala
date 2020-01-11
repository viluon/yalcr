import scala.io.StdIn

object Main extends App {
  @scala.annotation.tailrec
  def run(history: List[String] = Nil): Unit = {
    val cmd = (StdIn readLine "λ ").toLowerCase()

    false match {
      case _ if cmd startsWith "solve " =>
        val expr = cmd drop "solve ".length
        println(solve(expr))
        run(expr :: history)

      case _ if cmd startsWith "help" =>
        println(usage)
        run(history)

      case _ =>
        println(s"unknown command $cmd (try 'help')")
        run(history)
    }
  }

  val usage: String = "available commands are 'solve [expr]' and 'help'"

  def solve(expr: String): String = {
    val r = Parser.parseAll(Parser.expression, expr)

    if (r.successful) {
      val expr = r.get
      val e = Reductions beta expr
      expr.pretty() + "\n" + (e match {
        case Some(reduced) => reduced.pretty()
        case None => "None"
      })
    } else r.toString
  }

  run()
}
