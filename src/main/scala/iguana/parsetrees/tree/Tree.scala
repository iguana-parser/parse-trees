package iguana.parsetrees.tree

import iguana.parsetrees.slot.TerminalNodeType
import iguana.utils.input.Input

import scala.collection.JavaConverters._

trait Tree {
  def leftExtent: Int
  def rightExtent: Int

  def equals(t: Tree): Boolean
}

object TreeFactory {
  def createRule(ruleType: RuleType, children: java.util.List[Tree], input: Input) = RuleNode(ruleType, children.asScala, input)
  def createAmbiguity(children: java.util.List[java.util.List[Tree]]) = Amb(asScala(children))
  def createTerminal(terminalType: TerminalType, leftExtent: Int, rightExtent:Int, input: Input) = Terminal(terminalType, leftExtent, rightExtent, input)
  def createEpsilon(i: Int) = Epsilon(i)
  def createCycle(label: String) = Cycle(label)
  def createStar(children: java.util.List[Tree]) = Star(children.asScala)
  def createPlus(children: java.util.List[Tree]) = Plus(children.asScala)
  def createGroup(children: java.util.List[Tree]) = Group(children.asScala)
  def createAlt(children: java.util.List[Tree]) = Alt(children.asScala)
  def createOpt(child: Tree) = Opt(child)


  def asScala[T](list: java.util.List[java.util.List[T]]): Seq[Seq[T]] = list.asScala.map(x => x.asScala)
}

class RuleNode(val r: RuleType, val ts: Seq[Tree], val input: Input) extends Tree {
  override def leftExtent= ts.head.leftExtent
  override def rightExtent = ts.last.rightExtent
  def isLayout: Boolean = r.layout

  override def equals(t: Tree): Boolean =  t match {
    case n: RuleNode => leftExtent == n.leftExtent &&
                        rightExtent == n.rightExtent &&
                        r.head  == n.r.head &&
                        ts.zip(n.ts).forall { case (t1, t2) => t1 equals t2 }
    case _           => false
  }
}

object RuleNode {
  def apply(r: RuleType, ts: Seq[Tree], input: Input) = new RuleNode(r, ts, input)
  def unapply(r: RuleNode): Option[(RuleType, Seq[Tree], Input)] = Some(r.r, r.ts, r.input)
}


class Amb(val ts: Seq[Seq[Tree]]) extends Tree {
  override def leftExtent: Int = ts.head.head.leftExtent
  override def rightExtent: Int = ts.head.last.rightExtent

  override def equals(t: Tree): Boolean = t match {
    case a: Amb => leftExtent == a.leftExtent &&
                   rightExtent == a.rightExtent &&
                   ts.zip(a.ts).forall { case (t1, t2) => t1.zip(t2).forall { case (z1, z2) => z1 equals z2 } }
    case _      => false
  }
}

object Amb {
  def apply(ts: Seq[Seq[Tree]]) = new Amb(ts)
  def unapply(a: Amb): Option[Seq[Seq[Tree]]] = Some(a.ts)
}

class Terminal(val tt: TerminalType, val leftExtent: Int, val rightExtent: Int, val input: Input) extends Tree {
  def isLayout: Boolean = tt.nodeType == TerminalNodeType.Layout

  override def equals(t: Tree): Boolean = t match {
    case t:Terminal => leftExtent == t.leftExtent &&
                       rightExtent == t.rightExtent &&
                       tt == t.tt
    case _          => false
  }
}

object Terminal {

  def apply(tt: TerminalType, leftExtent: Int, rightExtent: Int, input: Input) =
    new Terminal(tt, leftExtent, rightExtent, input)

  def unapply(t: Terminal): Option[(TerminalType, Int, Int, Input)] = Some(t.tt, t.leftExtent, t.rightExtent, t.input)

}

class Epsilon(val i: Int) extends Tree {
  override def leftExtent = i
  override def rightExtent = i

  override def equals(t: Tree): Boolean = t match {
    case e: Epsilon => i == e.i
    case _          => false
  }
}

object Epsilon {
  def apply(i: Int) = new Epsilon(i)
  def unapply(e: Epsilon): Option[Int] = Some(e.i)
}

class Cycle(val label: String) extends Tree {
  override def leftExtent = -1
  override def rightExtent = -1

  override def equals(t: Tree): Boolean = t match {
    case c:Cycle => label == c.label
    case _       => false
  }
}

object Cycle {
  def apply(label: String) = new Cycle(label)
  def unapply(c: Cycle): Option[String] = Some(c.label)
}

class Star(val children: Seq[Tree]) extends Tree {
  override def leftExtent = children.head.leftExtent
  override def rightExtent = children.last.rightExtent

  override def equals(t: Tree): Boolean = t match {
    case s:Star => children.zip(s.children).forall { case (t1, t2) => t1 equals t2 }
    case _      => false
  }
}

object Star {
  def apply(children: Seq[Tree]) = new Star(children)
  def unapply(s: Star): Option[Seq[Tree]] = Some(s.children)
}

class Plus(val children: Seq[Tree]) extends Tree {
  override def leftExtent = children.head.leftExtent
  override def rightExtent = children.last.rightExtent

  override def equals(t: Tree): Boolean = t match {
    case p:Plus => children.zip(p.children).forall { case (t1, t2) => t1 equals t2 }
    case _      => false
  }
}

object Plus {
  def apply(children: Seq[Tree]) = new Plus(children)
  def unapply(p: Plus): Option[Seq[Tree]] = Some(p.children)
}

class Group(val children: Seq[Tree]) extends Tree {
  override def leftExtent = children.head.leftExtent
  override def rightExtent = children.last.rightExtent

  override def equals(t: Tree): Boolean = t match {
    case g:Group => children.zip(g.children).forall { case (t1, t2) => t1 equals t2 }
    case _      => false
  }
}

object Group {
  def apply(children: Seq[Tree]) = new Group(children)
  def unapply(g: Group): Option[Seq[Tree]] = Some(g.children)
}

class Opt(val child: Tree) extends Tree {
  override def leftExtent = child.leftExtent
  override def rightExtent = child.rightExtent

  override def equals(t: Tree): Boolean = t match {
    case o:Opt => child.equals(o.child)
    case _      => false
  }
}

object Opt {
  def apply(child: Tree) = new Opt(child)
  def unapply(o: Opt): Option[Tree] = Some(o.child)
}

class Alt(val children: Seq[Tree]) extends Tree {
  override def leftExtent = children.head.leftExtent
  override def rightExtent = children.last.rightExtent

  override def equals(t: Tree): Boolean = t match {
    case a:Alt => children.zip(a.children).forall { case (t1, t2) => t1 equals t2 }
    case _      => false
  }
}

object Alt {
  def apply(children: Seq[Tree]) = new Alt(children)
  def unapply(a: Alt): Option[Seq[Tree]] = Some(a.children)
}

