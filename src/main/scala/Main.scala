
object Main extends App {
  println(Parser.parseAll(Parser.expression, "(λ x. (1 2) (3 4)) 5"))
}
