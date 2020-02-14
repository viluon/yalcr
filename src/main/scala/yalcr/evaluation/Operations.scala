package yalcr.evaluation

object Operations extends Enumeration {
  type Operation = Operations.Value

  val betaReduction: Operation = Value("β reduction")
  val macroExpansion: Operation = Value("macro expansion")
  val identity: Operation = Value("identity")
}
