package yalcr.lang

sealed trait Expression {
  def pretty(depth: Int = 0, context: Set[String] = Set.empty): String

  val simple: Boolean

  protected[lang] def nice(implicit depth: Int, context: Set[String]): String = pretty(depth + 1, context)

  def coloured(text: String, depth: Int): String = {
    val colour = List(Console.GREEN
                    , Console.YELLOW
                    , Console.BLUE
                    , Console.MAGENTA
                    , Console.CYAN
                    , Console.WHITE)(depth % 6)

    s"$colour$text${Console.RESET}"
  }

  def parens(text: String)(implicit depth: Int): String = coloured("(", depth) + text + coloured(")", depth)

  def wrong(text: String): String = s"${Console.RED_B + Console.BLACK}$text${Console.RESET}"
}

case class ENumber(num: Int) extends Expression {
  override def pretty(depth: Int, context: Set[String]): String = s"$num"

  override val simple: Boolean = true
}

case class EParam(name: String) extends Expression {
  override def pretty(depth: Int, context: Set[String]): String = if (context contains name) name else wrong(name)

  override def toString: String = name

  override val simple: Boolean = true
}

case class ELambda(params: List[EParam], body: Expression) extends Expression {
  override def pretty(depth: Int, context: Set[String]): String = {
    implicit val d: Int = depth
    parens(s"λ ${params mkString ", "}. ${body.nice(depth, params.foldLeft(context)((ctx, p) => ctx incl p.name))}")
  }

  override val simple: Boolean = true
}

case class EApplication(λ: Expression, argument: Expression) extends Expression {
  override def pretty(depth: Int, context: Set[String]): String = {
    implicit val d: Int = depth
    implicit val c: Set[String] = context
    val t = s"${λ.nice} ${argument.nice}"
    if (argument.simple) t else parens(t)
  }

  override val simple: Boolean = false
}
