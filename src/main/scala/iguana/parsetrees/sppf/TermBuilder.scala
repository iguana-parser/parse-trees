package iguana.parsetrees.sppf

trait TermBuilder {
  def terminalNode(s: Any, l: Int, r: Int)
  def layoutNode(s: Any, l: Int, r: Int)
  def nonterminalNode(s: Any, l: Int, r: Int)
  def ambiguityNode(children: Set[Any], l:Int, r:Int)
}
