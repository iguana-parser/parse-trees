package iguana.parsetrees.tree

import scala.collection.JavaConverters._

trait Tree

object Tree {
  val epsilon = Epsilon()
  def isEpsilon(t: Tree): Boolean = t == epsilon
}

object TreeFactory {
  def createRule(ruleType: Any, children: java.util.List[Tree]) = RuleNode(ruleType, children.asScala)
  def createAmbiguity(children: Set[Tree]) = Amb(children)
  def createTerminal(value: String) = Terminal(value)
  def createLayout(t: Tree) = Layout(t)
  def createEpsilon() = Epsilon()
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

case class Amb(ts: Set[Tree]) extends Tree

case class Terminal(value: String) extends Tree

case class Layout(value: Tree) extends Tree

case class Epsilon() extends Tree

