package iguana.parsetrees.tree

import scala.collection.convert.wrapAsScala._
import scala.collection.mutable.Set

trait Tree

object TreeFactory {
  def createRule(ruleType: Any, children: java.util.List[Tree]) = RuleNode(ruleType, children)
  def createAmbiguity(children: java.util.Set[Branch[Tree]]) = Amb(children)
  def createBranch(children: java.util.List[Tree]) = TreeBranch(children)
  def createTerminal(value: String) = Terminal(value)
  def createEpsilon() = Epsilon()
  def createCycle() = Cycle()
  def createLayout(t: Tree) = Layout(t)
}

trait RuleNode extends Tree {
  def r: Any
  def ts: Seq[Tree]
}

case class RuleNodeImpl(val r: Any, val ts: Seq[Tree]) extends RuleNode

object RuleNodeL {
  def unapply(n: RuleNode): Option[(Any, Seq[Tree])] = Some((n.r, n.ts))
}

object RuleNode {
  def apply(r: Any, ts: Seq[Tree])= new RuleNodeImpl(r, ts)
  def unapply(n: RuleNode): Option[(Any, Seq[Tree])] = Some((n.r, n.ts filter { case l: Layout => false; case _ => true }))
}

case class TreeBranch(children: Seq[Tree]) extends Branch[Tree]

case class Amb(ts: Set[Branch[Tree]]) extends Tree

case class Terminal(value: String) extends Tree

case class Layout(value: Tree) extends Tree

case class Epsilon() extends Tree

case class Cycle() extends Tree

