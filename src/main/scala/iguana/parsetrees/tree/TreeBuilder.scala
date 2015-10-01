package iguana.parsetrees.tree

import iguana.utils.input.Input
import scala.collection.mutable.Set

/**
 * Represents an ambiguity branch
 */
trait Branch[T] {
  def children: Seq[T]
}

trait TreeBuilder[T] {
  def terminalNode(l: Int, r: Int): T
  def nonterminalNode(ruleType: Any, children: Seq[T], l: Int, r: Int): T
  def ambiguityNode(children: Set[Branch[T]], l:Int, r:Int): T
  def branch(children: Seq[T]): Branch[T]
  def cycle(): T
  def epsilon(): T
}

object TreeBuilderFactory {
  def getDefault(input: Input) = new DefaultTreeBuilder(input)
}

class DefaultTreeBuilder(input: Input) extends  TreeBuilder[Tree] {

  override def terminalNode(l: Int, r: Int): Tree = Terminal(input.subString(l, r))

  //  def layoutNode(s: Any, l: Int, r: Int): T
  override def nonterminalNode(ruleType: Any, children: Seq[Tree], l: Int, r: Int): Tree = RuleNode(ruleType, children)

  override def ambiguityNode(children: Set[Branch[Tree]], l: Int, r: Int): Tree = Amb(children)

  override def branch(children: Seq[Tree]): Branch[Tree] = TreeBranch(children)

  override def cycle() = Cycle()

  override def epsilon(): Tree = Epsilon()
}
