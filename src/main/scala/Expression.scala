sealed trait Expression {
  def pretty(depth: Int = 0, context: Set[String] = Set.empty): String

  def coloured(text: String, depth: Int): String = {
    val colour = List(Console.GREEN
                    , Console.YELLOW
                    , Console.BLUE
                    , Console.MAGENTA
                    , Console.CYAN
                    , Console.WHITE)(depth % 6)

    s"$colour$text${Console.RESET}"
  }

  def parens(text: String, depth: Int): String = coloured("(", depth) + text + coloured(")", depth)

  def wrong(text: String): String = s"${Console.RED_B + Console.BLACK}$text${Console.RESET}"
}

case class ENumber(num: Int) extends Expression {
  override def pretty(depth: Int, context: Set[String]): String = s"$num"
}

case class EParam(name: String) extends Expression {
  override def pretty(depth: Int, context: Set[String]): String = if (context contains name) name else wrong(name)

  override def toString: String = name
}

case class ELambda(params: List[EParam], body: Expression) extends Expression {
  override def pretty(depth: Int, context: Set[String]): String = {
    parens(s"λ ${params mkString ", "}. ${body.pretty(depth + 1, params.foldLeft(context)((ctx, p) => ctx incl p.name))}", depth)
  }
}

case class EApplication(λ: Expression, param: Expression) extends Expression {
  override def pretty(depth: Int, context: Set[String]): String = parens(s"${λ.pretty(depth + 1, context)} ${param.pretty(depth + 1, context)}", depth)
}
