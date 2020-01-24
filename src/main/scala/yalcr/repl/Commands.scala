package yalcr.repl
import yalcr.extensions.Strings.newline

object Commands extends Enumeration {
  type Command = Commands.Value

  lazy val usage: String = s"Available commands: $newline" + ((values map ("\t" + _.toString)) mkString newline)

  val solve: Command = Value
  val help: Command = Value
}
