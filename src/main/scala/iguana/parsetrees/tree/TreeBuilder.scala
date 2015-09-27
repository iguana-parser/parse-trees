package iguana.parsetrees.tree

import iguana.utils.input.Input

trait TreeBuilder[T] {

  def terminalNode(l: Int, r: Int): T

  def nonterminalNode(ruleType: Any, children: Seq[T], l: Int, r: Int): T

  def ambiguityNode(children: Set[T], l:Int, r:Int): T
}

object TreeBuilderFactory {
  def getDefault(input: Input) = new DefaultTreeBuilder(input)
}

class DefaultTreeBuilder(input: Input) extends  TreeBuilder[Tree] {

  override def terminalNode(l: Int, r: Int): Tree = Terminal(input.subString(l, r))

  //  def layoutNode(s: Any, l: Int, r: Int): T
  override def nonterminalNode(ruleType: Any, children: Seq[Tree], l: Int, r: Int): Tree = RuleNode(ruleType, children)

  override def ambiguityNode(children: Set[Tree], l: Int, r: Int): Tree = Amb(children)

}
