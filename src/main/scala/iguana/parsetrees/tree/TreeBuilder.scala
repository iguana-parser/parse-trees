package iguana.parsetrees.tree

import iguana.utils.input.Input
import iguana.parsetrees.slot.Action

/**
 * Represents an ambiguity branch
 */
trait Branch[T] {
  def ruleType: RuleType
  def children: Seq[T]
  def leftExtent: Int
  def rightExtent: Int
}

trait RuleType {
  def head: String
  def body: java.util.List[String]
  def action: Action
  def position: Int
}

trait TreeBuilder[T] {
  def terminalNode(l: Int, r: Int, input: Input): T
  def terminalNode(name: String, l: Int, r: Int, input: Input): T
  def nonterminalNode(ruleType: RuleType, children: Seq[T], l: Int, r: Int, input: Input): T
  def ambiguityNode(children: Iterable[Branch[T]], l:Int, r:Int): T
  def branch(r: RuleType, children: Seq[T]): Branch[T]
  def star(children: Seq[T]): T
  def plus(children: Seq[T]): T
  def alt(l: Seq[T]): T
  def opt(child: T): T
  def group(children: Seq[T]): T
  def cycle(label: String): T
  def epsilon(i: Int): T
}

object TreeBuilderFactory {
  def getDefault(input: Input) = new DefaultTreeBuilder(input)
}

class DefaultTreeBuilder(input: Input) extends TreeBuilder[Tree] {

  type T = Tree

  override def terminalNode(l: Int, r: Int, input: Input): Tree = Terminal(l, r, input)

  override def terminalNode(name: String, l: Int, r: Int, input: Input): Tree = Terminal(name, l, r, input)

  override def nonterminalNode(ruleType: RuleType, children: Seq[Tree], l: Int, r: Int, input: Input): Tree =
    RuleNode(ruleType, children, input)

  override def ambiguityNode(children: Iterable[Branch[Tree]], l: Int, r: Int): Tree = Amb(children.toSet[Branch[Tree]])

  override def branch(r: RuleType, children: Seq[Tree]): Branch[Tree] = TreeBranch(r, children)

  override def cycle(label: String) = Cycle(label)

  override def epsilon(i: Int): Tree = Epsilon(i)

  override def star(children: Seq[Tree]): Tree = Star(children)

  override def plus(children: Seq[Tree]): Tree = Plus(children)

  override def group(children: Seq[Tree]): Tree = Group(children)

  override def alt(children: Seq[Tree]): Tree = Alt(children)

  override def opt(child: Tree): Tree = Opt(child)

}
