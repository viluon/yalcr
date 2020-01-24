package yalcr.repl.eval

trait Strategy[State, Command] {
  def eval(state: State, cmd: Command): State
}
