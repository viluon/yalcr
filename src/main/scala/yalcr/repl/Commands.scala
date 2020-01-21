package yalcr.repl

object Commands extends Enumeration {
  type Command = Commands.Value

  val usage: String = s"Available commands: ${System.lineSeparator()}" + values mkString System.lineSeparator()

  val solve: Command = Value
  val help: Command = Value
}
