package iguana.parsetrees.tree

import scala.collection.convert.wrapAsScala._
import scala.collection.mutable.Set

trait Tree {
  def leftExtent: Int
  def rightExtent: Int
}

object TreeFactory {
  def createRule(ruleType: Any, children: java.util.List[Tree]) = RuleNode(ruleType, children)
  def createAmbiguity(children: java.util.Set[Branch[Tree]]) = Amb(children)
  def createBranch(children: java.util.List[Tree]) = TreeBranch(children)
  def createTerminal(leftExtent: Int, rightExtent:Int) = Terminal(leftExtent, rightExtent)
  def createEpsilon(i: Int) = Epsilon(i)
  def createCycle() = Cycle()
  def createLayout(t: Tree) = Layout(t)
}

trait RuleNode extends Tree {
  def r: Any
  def ts: Seq[Tree]
  override def leftExtent= ts.head.leftExtent
  override def rightExtent = ts.last.rightExtent
}

case class RuleNodeImpl(val r: Any, val ts: Seq[Tree]) extends RuleNode

object RuleNodeL {
  def unapply(n: RuleNode): Option[(Any, Seq[Tree])] = Some((n.r, n.ts))
}

object RuleNode {
  def apply(r: Any, ts: Seq[Tree])= new RuleNodeImpl(r, ts)
  def unapply(n: RuleNode): Option[(Any, Seq[Tree])] = Some((n.r, n.ts filter { case l: Layout => false; case _ => true }))
}

case class TreeBranch(children: Seq[Tree]) extends Branch[Tree] {
  override def leftExtent = children.head.leftExtent
  override def rightExtent = children.last.rightExtent
}

case class Amb(ts: Set[Branch[Tree]]) extends Tree {
  override def leftExtent: Int = ts.head.leftExtent
  override def rightExtent: Int = ts.head.rightExtent
}

case class Terminal(leftExtent: Int, rightExtent: Int) extends Tree

case class Layout(t: Tree) extends Tree {
  override def leftExtent = t.leftExtent
  override def rightExtent = t.rightExtent
}

case class Epsilon(i: Int) extends Tree {
  override def leftExtent = i
  override def rightExtent = i
}

case class Cycle() extends Tree {
  override def leftExtent = throw new RuntimeException("Should not be called!")
  override def rightExtent = throw new RuntimeException("Should not be called!")
}

