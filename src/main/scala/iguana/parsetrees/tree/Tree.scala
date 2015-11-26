package iguana.parsetrees.tree

import iguana.utils.input.Input

import scala.collection.convert.wrapAsScala._
import scala.collection.immutable.Set

trait Tree {
  def leftExtent: Int
  def rightExtent: Int
}

object TreeFactory {
  def createRule(ruleType: RuleType, children: java.util.List[Tree], input: Input) = RuleNode(ruleType, children, input)
  def createAmbiguity(children: java.util.Set[Branch[Tree]]) = Amb(children.toSet)
  def createBranch(r: RuleType, children: java.util.List[Tree]) = TreeBranch(r, children)
  def createBranch(children: java.util.List[Tree]) = TreeBranch(null, children)
  def createTerminal(leftExtent: Int, rightExtent:Int, input: Input) = Terminal(leftExtent, rightExtent, input)
  def createEpsilon(i: Int) = Epsilon(i)
  def createCycle(label: String) = Cycle(label)
  def createLayout(t: Tree) = Layout(t)
  def createStar(children: java.util.List[Tree]) = Star(children)
  def createPlus(children: java.util.List[Tree]) = Plus(children)
  def createGroup(children: java.util.List[Tree]) = Group(children)
  def createAlt(children: java.util.List[Tree]) = Alt(children)
  def createOpt(child: Tree) = Opt(child)
}

case class RuleNode(val r: RuleType, val ts: Seq[Tree], input: Input) extends Tree {
  override def leftExtent= ts.head.leftExtent
  override def rightExtent = ts.last.rightExtent
}

case class TreeBranch(ruleType: RuleType, children: Seq[Tree]) extends Branch[Tree] {
  override def leftExtent = children.head.leftExtent
  override def rightExtent = children.last.rightExtent

  override def equals(o: Any) = o match {
    case TreeBranch(r, l) => l == children
    case _                => false
  }
}

case class Amb(ts: Set[Branch[Tree]]) extends Tree {
  override def leftExtent: Int = ts.head.leftExtent
  override def rightExtent: Int = ts.head.rightExtent
}

case class Terminal(name: String, leftExtent: Int, rightExtent: Int, input: Input) extends Tree

object Terminal {
  def apply(leftExtent: Int, rightExtent: Int, input: Input): Terminal =
    Terminal("DEFAULT", leftExtent, rightExtent, input)
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

