package iguana.parsetrees.tree

import scala.collection.convert.wrapAsScala._
import scala.collection.immutable.Set

trait Tree {
  def leftExtent: Int
  def rightExtent: Int
}

object TreeFactory {
  def createRule(ruleType: RuleType, children: java.util.List[Tree]) = RuleNode(ruleType, children)
  def createAmbiguity(children: java.util.Set[Branch[Tree]]) = Amb(children.toSet)
  def createBranch(children: java.util.List[Tree]) = TreeBranch(children)
  def createTerminal(leftExtent: Int, rightExtent:Int) = Terminal(leftExtent, rightExtent)
  def createEpsilon(i: Int) = Epsilon(i)
  def createCycle(label: String) = Cycle(label)
  def createLayout(t: Tree) = Layout(t)
  def createStar(children: java.util.List[Tree]) = Star(children)
  def createPlus(children: java.util.List[Tree]) = Plus(children)
  def createGroup(children: java.util.List[Tree]) = Group(children)
  def createAlt(children: java.util.List[Tree]) = Alt(children)
  def createOpt(child: Tree) = Opt(child)
}

case class RuleNode(val r: RuleType, val ts: Seq[Tree]) extends Tree {
  override def leftExtent= ts.head.leftExtent
  override def rightExtent = ts.last.rightExtent
}

case class TreeBranch(children: Seq[Tree]) extends Branch[Tree] {
  override def leftExtent = children.head.leftExtent
  override def rightExtent = children.last.rightExtent
}

case class Amb(ts: Set[Branch[Tree]]) extends Tree {
  override def leftExtent: Int = ts.head.leftExtent
  override def rightExtent: Int = ts.head.rightExtent
}

case class Terminal(name: String, leftExtent: Int, rightExtent: Int) extends Tree

object Terminal {
  def apply(leftExtent: Int, rightExtent: Int): Terminal = Terminal("DEFAULT", leftExtent, rightExtent)
}

case class Layout(t: Tree) extends Tree {
  override def leftExtent = t.leftExtent
  override def rightExtent = t.rightExtent
}

case class Epsilon(i: Int) extends Tree {
  override def leftExtent = i
  override def rightExtent = i
}

case class Cycle(label: String) extends Tree {
  override def leftExtent = throw new RuntimeException("Should not be called!")
  override def rightExtent = throw new RuntimeException("Should not be called!")
}

case class Star(children: Seq[Tree]) extends Tree {
  override def leftExtent = children.head.leftExtent
  override def rightExtent = children.last.rightExtent
}

case class Plus(children: Seq[Tree]) extends Tree {
  override def leftExtent = children.head.leftExtent
  override def rightExtent = children.last.rightExtent
}

case class Group(children: Seq[Tree]) extends Tree {
  override def leftExtent = children.head.leftExtent
  override def rightExtent = children.last.rightExtent
}

case class Opt(child: Tree) extends Tree {
  override def leftExtent = child.leftExtent
  override def rightExtent = child.rightExtent
}

case class Alt(children: Seq[Tree]) extends Tree {
  override def leftExtent = children.head.leftExtent
  override def rightExtent = children.last.rightExtent
}

